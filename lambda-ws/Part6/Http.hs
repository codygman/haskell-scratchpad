{-# LANGUAGE OverloadedStrings #-}

module Part6.Http (static, path) where

import Data.ByteString.Lazy.Char8 (pack)
import Network.Wai (Application, Request, responseLBS, pathInfo)
import Network.HTTP.Types (status200)
import Data.Text (unpack)

static :: String -> IO String
static fileName = readFile fileName

path :: Request -> String
path req = concatMap unpack $ pathInfo req

html :: String -> Application
html src _ = return $ responseLBS status200 [("Content-Type", "text/html")] $ pack src

