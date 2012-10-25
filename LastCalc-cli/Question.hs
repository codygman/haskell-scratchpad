{-# LANGUAGE OverloadedStrings #-}

module Question (Questions (..), encode) where

import Data.Aeson ((.=), object, Value (Object), ToJSON (toJSON))
import qualified Data.Aeson as A
import Data.Aeson.Types (Pair)
import Data.ByteString.Lazy.Internal (ByteString)
import Data.Text (pack)

import Data.Map (Map)

data Questions = Questions { questions :: [(String, String)] } deriving Show

instance ToJSON Questions where
  toJSON (Questions qs) = object ["questions" .= object (toJson qs) ] where
    toJson :: [(String, String)] -> [Pair]
    toJson []      = []
    toJson (q:qs') = ((pack ((fst q))) .= (snd q)) : (toJson qs')

encode :: Questions -> ByteString
encode = A.encode

