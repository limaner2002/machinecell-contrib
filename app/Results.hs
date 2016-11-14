{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE Arrows #-}

import ClassyPrelude hiding (filter)
import Control.Monad.Trans.Resource hiding (throwM)
import Data.Time.Clock.POSIX
import Data.Void (Void)
import Data.Time
import Control.Arrow
import MachineUtils
import ParseCSV
import Data.Default
import Control.Monad.State hiding (mapM_)
import System.FilePath.Glob
import System.FilePath

data JMeterReading = JMeterReading
  { timeStamp :: JMeterTimeStamp
  , elapsed :: Int
  , label :: Text
  , responseCode :: Int
  , responseMsg :: Text
  , threadName :: Text
  , dataType :: Text
  , success :: Bool
  , failureMessage :: Text
  , bytes :: Int
  , grpThreads :: Text
  , allThreads :: Int
  , latency :: Int
  , idleTime :: Int
  } deriving Show

data ExpressionDetails = ExpressionDetails
  { exprTimeStamp :: UTCTime
  , exprName :: Text
  , exprType :: Text
  , exprTotalCount :: Int
  , exprMeanTotalTimems :: Double
  , exprMinTotalTimems :: Int
  , exprMaxTotalTimems :: Int
  } deriving Show

showExpressionDetails :: ExpressionDetails -> Text
showExpressionDetails (ExpressionDetails ts name typ total mean min max)
  = tshow ts <> "\t" <> name <> "\t" <> typ <> "\t" <> tshow total <> "\t" <> tshow mean <> "\t" <> tshow min <> "\t" <> tshow max

newtype InvalidCSVEntry = InvalidCSVEntry Text
  deriving Show

instance Exception InvalidCSVEntry

newtype InvalidCSVRow = InvalidCSVRow Text
  deriving Show

instance Exception InvalidCSVRow

newtype JMeterTimeStamp = JMeterTimeStamp UTCTime
  deriving Show

data AggregateAccum = AggregateAccum Int Int
  deriving Show

type ReadMap = Map Text AggregateAccum

type Row a = [a]

fromRow :: MonadThrow m => Row Text -> m JMeterReading
fromRow [ts, elpsd, label, respCode, respMsg, threadName, dataType, success, failureMessage, bytes, grpThreads, allThreads, latency, idleTime]
  = JMeterReading
  <$> readJMeterTimeStamp ts
  <*> readThrow elpsd
  <*> pure label
  <*> readThrow respCode
  <*> pure respMsg
  <*> pure threadName
  <*> pure dataType
  <*> readLowerBool success
  <*> pure failureMessage
  <*> readThrow bytes
  <*> pure grpThreads
  <*> readThrow allThreads
  <*> readThrow latency
  <*> readThrow idleTime
fromRow row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

readJMeterTimeStamp ts = JMeterTimeStamp . posixSecondsToUTCTime <$> (\x -> fromIntegral x / 1000) <$> (readThrow ts)

exprDetailsFromRow :: MonadThrow m => Row Text -> m ExpressionDetails
exprDetailsFromRow [ts, name, typ, totalCount, meanTotalTime, minTotalTime, maxTotalTime] =
  ExpressionDetails <$> readTimeStamp ts
                    <*> pure name
                    <*> pure typ
                    <*> readThrow totalCount
                    <*> readThrow meanTotalTime
                    <*> readThrow minTotalTime
                    <*> readThrow maxTotalTime
 where
   readTimeStamp = parseTimeM False defaultTimeLocale "%e %b %Y %T %Z" . unpack
exprDetailsFromRow row = throwM $ InvalidCSVRow $ "Could not read row " <> tshow row

addReading :: JMeterReading -> ReadMap -> ReadMap
addReading reading readingMap = alterMap alterReading (label reading) readingMap
  where
    alterReading Nothing = Just (AggregateAccum 1 $ elapsed reading)
    alterReading (Just (AggregateAccum n et)) = Just (AggregateAccum (n+1) (et + elapsed reading))

readLowerBool :: MonadThrow m => Text -> m Bool
readLowerBool "false" = return False
readLowerBool "true" = return True
readLowerBool entry = throwM $ InvalidCSVEntry $ "Could not read value " <> entry

readThrow entry =
  case readMay entry of
    Nothing -> throwM $ InvalidCSVEntry $ "Could not read value " <> entry
    Just v -> return v

main :: IO ()
main = do
  (logPathGlob:outputDir:startTimeIn:endTimeIn:_) <- getArgs
  inFiles <- namesMatching $ unpack logPathGlob
  let eStartTime = readTime startTimeIn
      eEndTime = readTime endTimeIn
      outFiles = fmap (\x -> unpack outputDir </> takeFileName x) inFiles
  case eStartTime of
    Left err -> print err
    Right startTime -> case eEndTime of
      Left err' -> print err'
      Right endTime ->
        evalStateT (
          runRMachine_ ( readLogs startTime endTime ) (zip inFiles outFiles)
          ) Init
  where
    readTime :: Text -> Either SomeException JMeterTimeStamp
    readTime = readJMeterTimeStamp

readLogs :: (MonadResource m, MonadState FilterState m) => JMeterTimeStamp -> JMeterTimeStamp -> ProcessA (Kleisli m) (Event (FilePath, FilePath)) (Event ())
readLogs startTime endTime = proc input -> do
  mX <- evMap Just >>> hold Nothing -< input
  case mX of
    Nothing -> returnA -< noEvent
    Just (inFile, outFile) -> do
      eDta <- edge >>> anytime (passthroughK print) >>> processLog startTime endTime >>> evMap ((<> "\n") . showExpressionDetails . unsafeFromEither) -< inFile
      res <- (edge *** evMap id) >>> sinkFile_ -< (outFile, eDta)
      returnA -< res
  where
    processLog startTime endTime =
          sourceFile
      >>> evMap asText
      >>> machineParser (parseRow def)
      >>> dropHeader
      >>> evMap (join . mapM exprDetailsFromRow)
      >>> filter (Kleisli $ filterTime 1800 startTime endTime)
    unsafeFromEither (Right v) = v

isBetween_
  :: JMeterTimeStamp
     -> JMeterTimeStamp -> Either t ExpressionDetails -> Bool
isBetween_ start end (Left _) = False
isBetween_ start end (Right v) = isBetween start end v

isBetween
  :: JMeterTimeStamp -> JMeterTimeStamp -> ExpressionDetails -> Bool
isBetween (JMeterTimeStamp startTime) (JMeterTimeStamp endTime) exprDetails
  = startTime <= time && time <= endTime
  where
    time = exprTimeStamp exprDetails

filterTime
  :: MonadState FilterState m =>
     NominalDiffTime
     -> JMeterTimeStamp
     -> JMeterTimeStamp
     -> Either t ExpressionDetails
     -> m Bool
filterTime logTimeDiff start end (Left _) = return False
filterTime logTimeDiff start end (Right v) = filterTime_ logTimeDiff start end v

                                             -- Stateful filter to get timestamps that occurred after the endtime in the log file
filterTime_ :: MonadState FilterState m => NominalDiffTime -> JMeterTimeStamp -> JMeterTimeStamp -> ExpressionDetails -> m Bool
filterTime_ logTimeDiff start@(JMeterTimeStamp startTime) end@(JMeterTimeStamp endTime) exprDetails = do
  filterState <- get
  case filterState of
    Init -> case isBetween start end exprDetails of
      False -> case endTime < (exprTimeStamp exprDetails) of
        False -> return False
        True -> put (GatherAllLast $ exprTimeStamp exprDetails) >> return True
      True -> put Between >> return True
    Between -> case isBetween start end exprDetails of
      True -> return True
      False -> put (GatherAllLast $ exprTimeStamp exprDetails) >> return True
    GatherAllLast ts ->
      case diffUTCTime (exprTimeStamp exprDetails) ts > logTimeDiff of
        False -> return True
        True -> put Done >> return False
    Done -> return False

asEither :: Either SomeException a -> Either SomeException a
asEither = id

getCurrMax :: (Monad m, Ord a) => ProcessA (Kleisli m) (Event a) (Event a)
getCurrMax = constructT kleisli0 $ loop Nothing
  where
    loop Nothing = do
      x <- await
      yield x
      loop (Just x)
    loop (Just v) = do
      x <- await
      case x > v of
        True -> do
          yield x
          loop (Just x)
        False -> do
          yield v
          loop (Just v)



data FilterState
  = Init
  | Between
  | GatherAllLast UTCTime
  | Done
  deriving Show
