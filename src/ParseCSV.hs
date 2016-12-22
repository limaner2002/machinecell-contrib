{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE Arrows #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module ParseCSV where

import ClassyPrelude hiding (throwM)
import Control.Arrow
import Control.Arrow.Machine
import Data.Attoparsec.Text hiding (take)
import qualified Data.Attoparsec.Text as AT
import qualified Data.Attoparsec.ByteString as AB
import qualified Data.Attoparsec.Types as A
import Data.Default
import Control.Monad.Trans.Resource
import qualified Data.Aeson as AE

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

class AttoparsecInput a where
  parseA :: A.Parser a b -> a -> A.IResult a b

instance AttoparsecInput Text where
  parseA = AT.parse

instance AttoparsecInput ByteString where
  parseA = AB.parse

instance MonadThrow AE.Result where
  throwM e = AE.Error (show e)

machineParser :: (Monoid a, MonoFoldable a, AttoparsecInput a, MonadThrow m, Monad m1) => A.Parser a b -> ProcessA (Kleisli m1) (Event a) (Event (m b))
machineParser p = constructT kleisli0 $ go (parseA p)
  where
    handleIt end (Done rest x) = do
      yield $ return x
      case onull rest of
        True -> case end of
          True -> return ()
          False -> go (parseA p)
        False -> handleIt end (parseA p rest)
    handleIt end (Partial f)
      | end = return ()
      | otherwise = go f
    handleIt end (Fail lo x s) = yield $ throwM $ ParseError s
    go parser = do
      txt <- await `catchP` (return mempty)
      let p = parser txt
      handleIt (onull txt) p

parseRow csvSettings = manyTill' (parseCell csvSettings) (endOfLine <|> endOfInput)

                       -- Does not parse escaped quotes correctly. Returns the two quote chars instead of the individual character.
parseCell :: CSVSettings -> A.Parser Text Text
parseCell csvSettings = parseQuoted <|> parseValue
  where
    parseQuoted = parseQuote csvSettings *> takeTill (\c -> c == csvQuote csvSettings || isEndOfLine c) <* (parseQuote csvSettings >> (optional $ char (separator csvSettings)))
    parseValue = takeTill (\c -> c == separator csvSettings || isEndOfLine c) <* (optional $ char (separator csvSettings))

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

parseQuote :: CSVSettings -> Parser Char
parseQuote csvSettings = do
  c1 <- char quoteChar
  mC2 <- peekChar
  case mC2 of
    Nothing -> return c1
    Just c2 -> case c2 == quoteChar of
      True -> fail "Quoted!"
      False -> return c1
  where
    quoteChar = csvQuote csvSettings
