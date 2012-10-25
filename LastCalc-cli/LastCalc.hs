{-# LANGUAGE OverloadedStrings #-}

module Main(main) where

--- $ curl -w "\n" -d "{\"questions\":{\"2\":\"2 + 2\"}}" localhost:8888/sl
--- {"answers":{"1":"4"},"answerTypes":{"1":"NORMAL"},"variables":{}}

import Answer
import Question
import Network.HTTP.Conduit
    ( http, parseUrl, withManager, RequestBody (RequestBodyLBS)
    , requestBody, method, Response (..)
    )
import Data.Conduit (($$))
import Control.Monad.IO.Class (liftIO)
import System (getArgs)
import Data.Conduit.List (consume)
import Data.HashMap ((!))

import qualified Data.ByteString.Lazy as BS (fromChunks)
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy.Char8 as LC

main :: IO ()
main = do
    args <- getArgs
    let valueBS = encode Questions { questions = [("1", (head args))] }
    req' <- parseUrl "http://www.lastcalc.com/sl"
    withManager $ \manager -> do
      let req = req' { method = "POST", requestBody = RequestBodyLBS valueBS }
      Response status headers body <- http req manager
      resValue <- body $$ consume
      let answers = parseAnswer $ C.pack $ LC.unpack $ BS.fromChunks resValue
      liftIO $ print answers

