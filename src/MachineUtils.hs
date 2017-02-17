{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE Strict #-}

module MachineUtils
  ( downloadHttp
  , downloadHttp_
  , inputCommand
  , inputCommand'
  , sink
  , createRequest
  , NothingYet (..)
  , machine
  , every
  , sampleOn
  , module Control.Arrow.Machine
  , module Control.Arrow
  , makeRequest
  , makeRequest_
  , runRMachine
  , runRMachine_
  , sourceHttp
  , sourceHttp_
  , sinkFile
  , sinkFile_
  , sourceFile
  , passthroughK
  , sourceDirectory
  , count
  , makeRowNum
  , RowNum
  , fromRowNum
  , sourceSocket
  , mergeEvents
  , showItSink
  , blockingSource'
  ) where

import ClassyPrelude hiding (first, race_, hPut, hGetChunk)
import Control.Arrow.Machine
import Control.Arrow
import Network
import qualified Network.Socket as NS
import qualified Data.Streaming.Network as SN
import Network.HTTP.Client
import Control.Monad.Trans.Resource
import Network.HTTP.Client.TLS
import qualified System.IO as SIO
import Network.HTTP.Simple
import Numeric
import System.Console.ANSI
import Control.Monad.Base
import qualified Data.Streaming.Filesystem as F
import Data.Streaming.Process
import Data.Time
import Data.Default
import Data.IOData

sink :: (Show a, MonadIO m) => (a -> m ()) -> ProcessA (Kleisli m) (Event a) (Event ())
sink act = repeatedlyT kleisli0 $ do
  x <- await
  lift $ act x

showItSink_ :: (Show a, MonadIO m) => a -> m ()
showItSink_ x = do
  liftIO $ do
    clearLine
    setCursorColumn 0
  putStr $ tshow x

showItSink :: (Show a, MonadIO m) => ProcessA (Kleisli m) (Event a) (Event ())
showItSink = machine showItSink_ >>> machine (const $ liftIO $ SIO.hFlush stdout)

sourceListen :: NS.Socket -> ProcessA (Kleisli IO) (Event ()) (Event (Socket, NS.SockAddr))
sourceListen sock = repeatedlyT kleisli0 $ do
  _ <- await
  accepted <- lift $ NS.accept sock
  yield accepted

sourceSocket :: ProcessA (Kleisli IO) (Event Socket) (Event ByteString)
sourceSocket = constructT kleisli0 go
  where
    go = do
      sock <- await
      loop sock
    loop sock = do
      bs <- lift $ SN.safeRecv sock 4096
      if null bs
        then do
          lift $ putStrLn "Socket is now empty"
          lift (NS.close sock)
          go
        else do
          yield bs
          loop sock

connectSocket_ :: MonadIO m => NS.AddrInfo -> m Socket
connectSocket_ addr = liftIO $ do
  sock <- NS.socket (NS.addrFamily addr) NS.Stream NS.defaultProtocol
  NS.connect sock (NS.addrAddress addr)
  return sock

decodeIt :: ArrowApply a => ProcessA a (Event ByteString) (Event Text)
decodeIt = repeatedly $ do
  x <- await
  yield $ decodeUtf8 x

sourceHttp :: MonadResource m => ProcessA
  (Kleisli m)
  (Event (Request, Manager))
  (Event ByteString)
sourceHttp = makeRequest >>> sourceHttp_ >>> evMap snd

data DownloadProgress = DownloadProgress TotalSize SoFar

instance Show DownloadProgress where
  show (DownloadProgress (TotalSize total) (SoFar soFar)) = showFFloat (Just 2) progress $ "%"
    where
      progress = (fromInteger soFar / fromInteger total) * 100

instance Default DownloadProgress where
  def = DownloadProgress (TotalSize 0) (SoFar 0)

newtype TotalSize = TotalSize Integer
newtype SoFar = SoFar Integer

getTotalSize :: Arrow a => a (Response BodyReader, ByteString) (Maybe TotalSize, ByteString)
getTotalSize = arr (fmap TotalSize . readMay . unpack . decodeUtf8 . concat . getResponseHeader "Content-Length") *** arr id

getProgress :: Monad m => PlanT (Maybe TotalSize, ByteString) (Maybe DownloadProgress, ByteString) m ()
getProgress = go 0
  where
    go n = do
      (totalSize, bs) <- await
      let chunkSize = toInteger $ length bs
          soFar = n + chunkSize
      yield (DownloadProgress <$> totalSize <*> pure (SoFar soFar), bs)
      go soFar

downloadHttp_ :: MonadResource m =>
     ProcessA
     (Kleisli m) (Event (Response BodyReader, ByteString)) (Event DownloadProgress, Event ByteString)
downloadHttp_ = anytime getTotalSize >>> construct getProgress
  >>> (sampleOn 100000 >>> evMap fst >>> evMap (fromMaybe def))
  &&& evMap snd

downloadHttp :: MonadResource m =>
  FilePath ->
     ProcessA
     (Kleisli m) (Event (Response BodyReader, ByteString)) (Event (), Event ())
downloadHttp fp = downloadHttp_  >>> showItSink
        	                 *** (sinkFile fp)

f :: ArrowApply a => ProcessA a (Event Integer) (Event Integer)
f = proc x ->
   do
     y <- accum 0 -< (+) <$> x
     returnA -< y <$ x

makeRequest :: MonadResource m => ProcessA (Kleisli m) (Event (Request, Manager)) (Event (ReleaseKey, Response BodyReader))
makeRequest = anytime makeRequest_

makeRequest_ :: MonadResource m => Kleisli m (Request, Manager) (ReleaseKey, Response BodyReader)
makeRequest_ = proc input -> do
  x <- Kleisli (\(req, mgr) -> allocate (responseOpen req mgr) responseClose) -< input
  returnA -< x

sourceHttp_ :: MonadResource m => ProcessA (Kleisli m) (Event (ReleaseKey, Response BodyReader)) (Event (Response BodyReader, ByteString))
sourceHttp_ = repeatedlyT kleisli0 go
  where
    go = do
      (key, res) <- await
      loop key res (responseBody res)
    loop key res bodyReader = do
      bs <- lift $ liftIO $ brRead $ bodyReader
      case null bs of
        True -> lift $ liftIO $ release key
        False -> do
          yield (res, bs)
          loop key res bodyReader

sinkFile :: (MonoFoldable a, MonadResource m, IOData a) => FilePath -> ProcessA (Kleisli m) (Event a) (Event ())
sinkFile fp = proc input -> do
  x <- edge -< fp
  res <- sinkFile_ -< (x, input)
  returnA -< res

testIt :: (MonoFoldable a, MonadResource m, IOData a) => FilePath -> ProcessA (Kleisli m) (Event a) (Event ())
testIt fp = proc input -> do
  x <- edge -< fp
  res <- sinkFile_ -< (x, input)
  returnA -< res

sinkFile_ :: (MonoFoldable a, MonadResource m, IOData a) => ProcessA (Kleisli m) (Event FilePath, Event a) (Event ())
sinkFile_ = tee >>> (constructT kleisli0 $ go Nothing)
  where
    go :: (MonoFoldable a, MonadResource m, IOData a) => Maybe (ReleaseKey, Handle) -> PlanT (Either FilePath a) () m r
    go mK = do
      x <- await
      case x of
        Left fp ->
          case mK of
            Nothing -> openNext fp
            Just (oldKey, _) -> do
              lift $ liftIO $ release oldKey
              openNext fp
        Right dta ->
          case mK of
            Nothing -> fail "There is no release key? This is most likely a bug."
            Just (key, handle) -> do
              lift . liftIO $ hPut handle dta
              go $ Just (key, handle)
    openNext fp = do
      k <- lift $ allocate (putStrLn "Opening file" >> SIO.openFile fp SIO.WriteMode) (\h -> putStrLn "Closing sinkfile handle" >> SIO.hFlush stdout >> SIO.hClose h)
      go $ Just k

sourceFile :: (MonoFoldable a, MonadResource m, IOData a) => ProcessA (Kleisli m) (Event FilePath) (Event a)
sourceFile = repeatedlyT kleisli0 go
  where
    go = do
      fp <- await
      (key, h) <- lift $ allocate (SIO.openFile fp SIO.ReadMode) SIO.hClose
      loop key h
    loop key h = do
      x <- lift $ liftIO $ hGetChunk h
      case onull x of
        True -> do
          lift $ liftIO $ release key
        False -> do
          yield x
          loop key h

sourceDirectory :: (MonadResource m, MonadMask m) => ProcessA (Kleisli m) (Event FilePath) (Event FilePath)
sourceDirectory = repeatedlyT kleisli0 go
  where
    go = do
      dir <- await
      (key, ds) <- lift $ allocate (F.openDirStream dir) (F.closeDirStream)
      loop dir ds
      lift . liftIO $ release key
    loop dir ds = do
      mfp <- lift . liftIO $ F.readDirStream ds
      case mfp of
        Nothing -> return ()
        Just fp -> do
          yield $ dir </> fp
          loop dir ds
      
inputCommand :: (MonadIO m, Read a) => ProcessA (Kleisli m) (Event ()) (Event a)
inputCommand = switch pred evt
  where
    -- pred :: (MonadIO m, Read a, Occasional a) => ProcessA (Kleisli m) (Event ()) (a, Event Text)
    pred :: MonadIO m => ProcessA (Kleisli m) (Event b) (Event a, Event Text)
    pred = proc evt -> do
      input <- machine (const $ liftIO $ getLine) >>> evMap asText -< evt
      returnA -< (noEvent, input)
    evt :: (MonadIO m, Read a) => Text -> ProcessA (Kleisli m) (Event ()) (Event a)
    evt "end" = construct stop
    evt input = case readMay input of
      Nothing -> errMsg input >>> inputCommand
      Just cmd -> evMap (const cmd)
    errMsg cmd = anytime (passthroughK $ const $ putStrLn $ "Invalid command: " <> cmd)
      
createRequest :: MonadThrow m => Manager -> ProcessA (Kleisli m) (Event String) (Event (Request, Manager))
createRequest mgr = constructT kleisli0 $ do
  url <- await
  req <- lift $ parseUrlThrow url
  yield (req, mgr)

data NothingYet = NothingYet
  deriving (Show, Eq)

instance Exception NothingYet

machine f = anytime (kleisli f)

sampleOn :: (MonadIO m, MonadBase IO m) => Int
  -> ProcessA (Kleisli m) (Event a) (Event a)
sampleOn ival = proc x -> do
  evt <- every ival -< x
  evt' <- tee >>> onEvent -< (x, evt)
  returnA -< evt'

onEvent :: Monad m => ProcessA (Kleisli m) (Event (Either a b)) (Event a)
onEvent = constructT kleisli0 go
  where
    go = do
      e <- await
      case e of
        Left x -> do
          yield x
          loop x
        Right _ -> go
    loop x = do
      mE <- (Just <$> await) `catchP` pure Nothing
      case mE of
        Nothing -> yield x >> stop
        Just e ->
          case e of
            Left x' -> loop x'
            Right _ -> do
              yield x
              loop x

every :: (MonadIO m, MonadBase IO m) => Int -> ProcessA (Kleisli m) (Event a) (Event ())
every ival = constructT kleisli0 go
  where
    go = do
      currentTime <- lift . liftIO $ getCurrentTime
      loop currentTime
    loop oldTime = do
      _ <- await
      currentTime <- lift . liftIO $ getCurrentTime
      let diffPicoSeconds = diffTimeToPicoseconds . fromRational . toRational $ diffUTCTime currentTime oldTime
      case diffPicoSeconds >= (fromIntegral ival) * 1000000 of
        True -> do
          yield ()
          loop currentTime
        False -> loop oldTime

runMachine :: Monad m => ProcessA (Kleisli m) (Event b) (Event c) -> [b] -> m [c]
runMachine = runKleisli . run

runMachine_ :: Monad m =>
     ProcessA (Kleisli m) (Event b) (Event c) -> [b] -> m ()
runMachine_ = runKleisli . run_

runRMachine
  :: MonadBaseControl IO m =>
     ProcessA (Kleisli (ResourceT m)) (Event b) (Event c)
     -> [b] -> m [c]
runRMachine machine input = runResourceT $ runMachine machine input

runRMachine_
  :: MonadBaseControl IO m =>
     ProcessA (Kleisli (ResourceT m)) (Event b) (Event c) -> [b] -> m ()
runRMachine_ machine input = runResourceT $ runMachine_ machine input

passthroughK :: (Monad m) => (a -> m ()) -> Kleisli m a a
passthroughK act = proc a -> do
  _ <- Kleisli act -< a
  returnA -< a

-- passthroughM = anytime . passthroughK

getInput :: MonadIO m => ProcessA (Kleisli m) (Event i) (Event Text)
getInput = anytime (Kleisli $ const $ putStrLn "This is the initial prompt!" >> return "hm!")

input :: MonadIO m => ProcessA (Kleisli m) (Event ()) (Event Text)
input = anytime $ kleisli0 $ liftIO getLine

checkInput :: (Read a, MonadIO m) => ProcessA (Kleisli m) (Event Text) (InputCmd (Maybe a))
checkInput = proc txtEvt -> do
  mTxt <- evMap Just >>> hold Nothing -< txtEvt
  case mTxt of
    Nothing -> returnA -< Init
    Just "end" -> returnA -< End
    Just txt -> arr readMay >>> arr Val >>> returnA -< txt

data InputCmd a
  = Init
  | End
  | Val a

inputCommand' :: (Read a, MonadIO m) => ProcessA (Kleisli m) (Event ()) (Event a)
inputCommand' = proc evt -> do
  input <- input >>> checkInput -< evt
  case input of
    Init -> returnA -< noEvent
    End -> construct stop -< evt
    (Val Nothing) -> anytime (kleisli0 $ putStrLn "Invalid command") >>> inputCommand -< evt
    (Val (Just v)) -> 
      returnA -< v <$ evt

count :: ArrowApply cat => ProcessA cat (Event a) (Event (Int, a))
count = proc input -> do
  n <- evMap (const (+1)) >>> accum 0 -< input
  returnA -< fmap (\x -> (n, x)) input

makeRowNum :: ArrowApply cat => ProcessA cat (Event a) (Event (RowNum, a))
makeRowNum = count >>> evMap (\(n, x) -> (RowNum n, x))

newtype RowNum = RowNum Int
  deriving (Show, Eq)

fromRowNum :: RowNum -> Int
fromRowNum (RowNum n) = n

-- Merge two events into one and fire when the second event fires
mergeEvents :: ArrowApply a => ProcessA a (Event b, Event c) (Event (b, c))
mergeEvents = proc (evtA, evtB) -> do
  mA <- evMap Just >>> hold Nothing -< evtA
  mB <- evMap Just >>> hold Nothing -< evtB
  case f mA mB of
    Nothing -> returnA -< noEvent
    Just (a, b) -> returnA -< (a, b) <$ evtB
  where
    f mA mB = (,) <$> mA <*> mB

-- merge :: ArrowApply cat => ProcessA cat (Event a, Event b) (Event (a, b))
-- merge = proc (evtA, evtB) -> do
--   mA <- evMap Just >>> hold Nothing -< evtA
--   mB <- evMap Just >>> hold Nothing -< evtB
--   case mA of
--     Nothing -> returnA -< noEvent
--     Just a -> case mB of
--       Nothing -> returnA -< noEvent
--       Just b -> returnA -< (a, b) <$ evtB

blockingSource' :: (ArrowApply a, MonoFoldable mono) => ProcessA a (Event mono) (Event (Element mono))
blockingSource' = repeatedly $ do
  l <- await
  mapM_ yield l
