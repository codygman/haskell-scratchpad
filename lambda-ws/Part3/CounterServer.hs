{-# LANGUAGE OverloadedStrings #-}

module Part3.CounterServer (main) where

import Control.Monad.State (liftIO)
import Data.ByteString.Lazy.Char8 (pack)
import Data.IORef (newIORef, atomicModifyIORef)
import Network.Wai (Application, Request, Response, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

app :: Int -> Request -> Response
app count req = responseLBS
  status200
  [("Content-Type", "application/json")]
  $ pack $ show count

main :: IO ()
main = do
  putStrLn $ "http://localhost:8000/"
  counter <- newIORef 1
  run 8000 $ (\req -> do
    count <- liftIO $ atomicModifyIORef counter (\a -> (a + 1, a))
    return $ app (fromIntegral count) req)

