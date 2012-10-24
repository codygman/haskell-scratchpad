{-# LANGUAGE OverloadedStrings #-}

module Part1.HelloWorldServer (main) where

import Network.Wai (Application, responseLBS)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)

app :: Application
app _ = return $ responseLBS
    status200
    [("Content-Type", "text/plain")]
    "Hello, World!"

main :: IO ()
main = do
    putStrLn $ "http://localhost:8000/"
    run 8000 app
