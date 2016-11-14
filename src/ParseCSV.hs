{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}

module ParseCSV where

import ClassyPrelude
import Control.Arrow
import Control.Arrow.Machine
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.Types as A
import Data.Default

data ParseError = ParseError String
  deriving Show

instance Exception ParseError

data CSVSettings = CSVSettings
  { separator :: Char
  , csvQuote :: Char
  , newlineChar :: Char
  } deriving Show

instance Default CSVSettings where
  def = CSVSettings ',' '"' '\n'

machineParser :: (MonadThrow m, Monad m1) => A.Parser Text b -> ProcessA (Kleisli m1) (Event Text) (Event (m b))
machineParser p = constructT kleisli0 $ go (parse p)
  where
    handleIt end (Done rest x) = do
      yield $ return x
      case onull rest of
        True -> case end of
          True -> return ()
          False -> go (parse p)
        False -> handleIt end (parse p rest)
    handleIt end (Partial f)
      | end = return ()
      | otherwise = go f
    handleIt end (Fail lo x s) = yield $ throwM $ ParseError s
    go parser = do
      txt <- await `catchP` (return mempty)
      let p = parser txt
      handleIt (onull txt) p

parseRow csvSettings = manyTill' (parseCell csvSettings) (endOfLine <|> endOfInput)

parseCell :: CSVSettings -> A.Parser Text Text
parseCell csvSettings = parseQuoted <|> parseValue
  where
    parseQuoted = char (csvQuote csvSettings) *> takeTill (\c -> c == csvQuote csvSettings || c == newlineChar csvSettings) <* (optional $ char (separator csvSettings))
    parseValue = takeTill (\c -> c == separator csvSettings || c == newlineChar csvSettings) <* (optional $ char (separator csvSettings))

dropHeader :: (MonadThrow m1, Monad m) => ProcessA (Kleisli m) (Event (m1 [Text])) (Event (m1 [Text]))
dropHeader = repeatedlyT kleisli0 $ do
  _ <- await
  loop
 where
   loop = do
     mRow <- (Just <$> await) `catchP` pure Nothing
     case mRow of
       Nothing -> return ()
       Just row -> do
         yield row
         loop
