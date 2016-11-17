{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE FlexibleContexts #-}

module MachineUtils
  ( downloadHttp
  , inputCommand
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
  , sourceHttp_
  , sinkFile
  , sinkFile_
  , sourceFile
  , passthroughK
  , sourceDirectory
  ) where

import ClassyPrelude hiding (first)
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

sink :: (Show a, MonadIO m) => (a -> m ()) -> ProcessA (Kleisli m) (Event a) (Event ())
sink act = repeatedlyT kleisli0 $ do
  x <- await
  lift $ act x

showItSink x = do
  liftIO $ do
    clearLine
    setCursorColumn 0
  putStr $ tshow x

sourceListen :: NS.Socket -> ProcessA (Kleisli IO) (Event ()) (Event (Socket, NS.SockAddr))
sourceListen sock = repeatedlyT kleisli0 $ do
  _ <- await
  accepted <- lift $ NS.accept sock
  yield accepted

sourceSocket :: ProcessA (Kleisli IO) (Event (Socket, NS.SockAddr)) (Event ByteString)
sourceSocket = constructT kleisli0 go
  where
    go = do
      (sock, addr) <- await
      lift $ putStrLn $ "Accepted connection from " <> tshow addr
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

downloadHttp :: MonadResource m =>
  FilePath -> 
     ProcessA
       -- (Kleisli m) (Event (Request, Manager)) (Event (), Event ())
     (Kleisli m) (Event (Response BodyReader, ByteString)) (Event (), Event ())
downloadHttp fp = anytime getTotalSize >>> construct getProgress
  >>> (evMap fst >>> machine showItSink >>> machine (const $ liftIO $ SIO.hFlush stdout))
  &&& (evMap snd >>> sinkFile fp)

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
sourceHttp_ = constructT kleisli0 go
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
            Nothing -> undefined
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
          stop
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
inputCommand = constructT kleisli0 go
  where
    go = do
      _ <- await
      input <- lift $ liftIO getLine
      case input of
        "end" -> stop
        _ -> case readMay input of
          Nothing -> do
            lift $ liftIO $ putStrLn $ "invalid command: " <> input
            go
          Just x -> do
            yield x
            go

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
  -> a
  -> ProcessA (Kleisli m) (Event a) (Event a)
sampleOn ival v = proc x -> do
  res <- hold v -< x
  sample <- evMap (const ()) >>> every ival -< x
  returnA -< res <$ sample

every :: (MonadIO m, MonadBase IO m) => Int -> ProcessA (Kleisli m) (Event a) (Event a)
every ival = constructT kleisli0 go
  where
    go = do
      v <- await
      lift $ liftIO $ threadDelay ival
      yield v
      go

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
