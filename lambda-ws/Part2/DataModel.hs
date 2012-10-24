{-# LANGUAGE OverloadedStrings #-}

module Part2.DataModel (Message(..)) where

import Data.Aeson (object, toJSON, (.=), ToJSON)

data Message = Message {
    id          :: Integer
  , messageText :: String
} deriving (Eq, Show)

instance ToJSON Message where
  toJSON (Message id messageText) = object [
      "id"      .= id
    , "message" .= messageText
    ]

