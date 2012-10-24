{-# LANGUAGE OverloadedStrings #-}

module Part6.ChatServer (main) where

import Part2.DataModel (Message(..))
import Control.Monad.State (liftIO)
import Control.Exception.Lifted (handle)
import Control.Exception (SomeException)
import Data.Aeson (json, decode, encode, object, toJSON)
import Data.Conduit (ResourceT, ($$))
import Data.Conduit.Attoparsec (sinkParser)
import Data.ByteString.Lazy.Char8 (pack)
import Data.ByteString.Char8 (unpack)
import Data.IORef (IORef, newIORef, atomicModifyIORef, readIORef)
import Network.Wai (Application, Request, Response, responseLBS, requestBody, requestMethod, pathInfo, queryString)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200, renderQuery)
import Data.Aeson.Types (Value)
import qualified Data.Text as T

main :: IO ()
main = do
  putStrLn $ "http://localhost:8000/"
  index <- readFile "Part5/index.html"
  messagesRef <- newIORef []
  run 8000 $ route index messagesRef

static :: String -> IO String
static fileName = do
  content <- readFile fileName
  return content

path :: Request -> String
path req = concatMap (T.unpack) $ pathInfo req

route :: String -> IORef [Value] -> Application
route index messagesRef req = case (pathInfo req) of
  [] -> html index req
  _  -> chat messagesRef req

html :: String -> Application
html src _ = return $ responseLBS
  status200
  [("Content-Type", "text/html")]
  $ pack src

chat :: IORef [Value] -> Application
chat messagesRef req = do
  newMessages <- parse req
  liftIO $ atomicModifyIORef messagesRef (\mRef -> (newMessages ++ mRef, mRef))
  messages <- liftIO $ readIORef messagesRef
  return $ responseLBS
    status200
    [("Content-Type", "application/json")]
    $ encode messages
      
parse :: Request -> ResourceT IO [Value]
parse req = do
  let query = queryString req
  let message = Message 1 $ unpack $ renderQuery False query
  let value = toJSON message
  return [value]

invalidJson :: SomeException -> ResourceT IO [Value]
invalidJson ex = return []

