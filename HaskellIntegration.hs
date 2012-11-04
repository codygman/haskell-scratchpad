-- HTTP, XML, RSS, and parsing in Haskell

import Text.XML.Light.Lexer (XmlSource)
import Text.XML.Light.Input (parseXML)
import Text.XML.Light.Types (Content, Element, Element (elName), QName (QName), QName (qName))
import Text.XML.Light.Proc (onlyElems, elChildren, findElements)
import System.IO (openFile, hPutStrLn, hClose, IOMode (WriteMode), hGetLine, hFlush)
import Network.HTTP (simpleHTTP, getRequest, getResponseBody)

-- data Message a = Message {payload :: a}

type URL = String

readFeed :: URL -> IO String
readFeed url = simpleHTTP (getRequest url) >>= getResponseBody

parseFeed :: XmlSource a => a -> [String]
parseFeed = map show . getItems where
  getItems xml = findElements (QName "item" Nothing Nothing) $ elemNamed "rss" $ parseXML xml

elemNamed :: String -> [Content] -> Element
elemNamed name = head . filter ((==name).qName.elName) . onlyElems

storeFeed :: String -> [String] -> IO ()
storeFeed f xs = do
  outFile <- openFile f WriteMode
  hPutStrLn outFile $ concatMap (++"\n") xs
  hClose outFile

main = do
  feed <- readFeed "http://lambda-the-ultimate.org/rss.xml"
  let items = parseFeed feed
  storeFeed "out" items

