{-# LANGUAGE OverloadedStrings #-}

module Part4.EchoServer (main) where

import Part2.DataModel (Message(..))
import Control.Monad.State (liftIO)
import Data.Aeson (json, decode, encode, object, toJSON)
import Data.Conduit (ResourceT, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.ByteString.Lazy.Char8 (pack)
import Data.IORef (newIORef, atomicModifyIORef, readIORef)
import Network.Wai (Application, Request, Response, responseLBS, requestBody)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Data.Aeson.Types (Value)

app :: [Value] -> Request -> Response
app messages req = responseLBS
  status200
  [("Content-Type", "application/json")]
  $ encode messages

main :: IO ()
main = do
  putStrLn $ "http://localhost:8000/"
  messagesRef <- newIORef []
  run 8000 $ (\req -> do
    message <- requestBody req $$ sinkParser json
    liftIO $ atomicModifyIORef messagesRef (\mRef -> (message : mRef, mRef))
    messages <- liftIO $ readIORef messagesRef
    return $ app messages req)

