{-# LANGUAGE OverloadedStrings #-}

module Answer (parseAnswer) where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Data.Attoparsec

import Data.Map (Map, elems)

import qualified Data.ByteString as B

data Results = Results { answers :: Map String String
                       , answerTypes :: Value
                       , variables :: Value
                       } deriving Show

instance FromJSON Results  where
  parseJSON (Object v) = Results <$>
                         v .: "answers" <*>
                         v .: "answerTypes" <*>
                         v .: "variables"
  parseJSON _          = mzero

data Answers = Map String String deriving Show

parseAnswers :: B.ByteString -> [Results]
parseAnswers s = case (parse (fromJSON <$> json) s) of
  Done _ (Error err)  -> error err
  Done ss (Success e) -> e:(parseAnswers ss)
  _                   -> []

parseAnswer :: B.ByteString -> String
parseAnswer s = head $ elems $ answers $ head $ parseAnswers s

main :: IO ()
main = B.readFile "json1.txt" >>= (\s -> print $ parseAnswer s)

