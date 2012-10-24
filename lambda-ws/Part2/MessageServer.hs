{-# LANGUAGE OverloadedStrings #-}

module Part2.MessageServer (main) where

import Part2.DataModel (Message(..))
import Data.Aeson (encode, object, toJSON)
import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

app :: Application
app req = return $ responseLBS
  status200
  [("Content-Type", "application/json")]
  $ encode $ toJSON $ Message 1 "Hello, World!"

main :: IO ()
main = do
  putStrLn $ "http://localhost:8000/"
  run 8000 app
