{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import qualified Data.Text.Encoding as T
import qualified Data.Text as T
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Data.Aeson
import Text.HTML.TagSoup
import Text.HTML.TagSoup.Match

main :: IO ()
main = do
    r2 <- parseHttp yURL parsePageListUL
    mapM_ (print . lbsToS) $ pageLinks r2

fetchVideoItem :: MonadIO m => Int -> m VideoItem
fetchVideoItem i = let aid = show i
                   in liftM (VideoItem aid) (httpGetTitle (getURL aid))

--validResult :: VideoItem -> Bool
--validResult (VideoItem _ t) = invalidResp == t

--invalidResp :: L.ByteString
--invalidResp = "良心网提示信息"

-----------------------------------------------------

outputToFile :: L.ByteString -> IO ()
outputToFile = L.writeFile outputFileName

outputFileName :: FilePath
outputFileName = "liangxin.json"

baseURL :: String
baseURL = "http://liangxin.net.cn/plus/view.php?aid="

yURL :: String
yURL = "http://liangxin.net.cn/plus/list.php?tid=78"

getURL :: String -> String
getURL i = baseURL ++ i


-------------------------------------------------------
type URL = String
type TitleTag = Tag L.ByteString
type TitleValue = L.ByteString

data VideoItem = VideoItem { vid :: String
                           , vtitle :: L.ByteString
                           }

instance ToJSON VideoItem where
    toJSON (VideoItem i t) = object ["vid" .= i, "vtitle" .= t]

titleTag :: L.ByteString
titleTag = "title"

parseTitleTags :: L.ByteString -> TitleTag
parseTitleTags doc = dropWhile (not . isTagOpenName titleTag) (parseTags doc) !! 1

httpGetTitle :: MonadIO m => URL -> m TitleValue
httpGetTitle = liftM (fromTagText . parseTitleTags) . simpleHttp

parseHttp :: MonadIO m => URL -> (L.ByteString -> [Tag L.ByteString]) -> m [Tag L.ByteString]
parseHttp url fn = liftM fn (simpleHttp url)


---------------- pagelist a

pageLinks :: [Tag L.ByteString] -> [L.ByteString]
pageLinks = map fromAttrHref . filter isPageLink
            where isPageLink = tagOpen (== "a") (hasAttr "href")

parsePageListUL :: L.ByteString -> [Tag L.ByteString]
parsePageListUL doc = takeWhile (not . isTagCloseName "ul") $
                      dropWhile (not . isOpenPageListUL) (parseTags doc)

isOpenPageListUL :: Tag L.ByteString -> Bool
isOpenPageListUL = tagOpen isUL hasPageListClass
                   where isUL = (== "ul")
                         hasPageListClass = anyAttr pageListClass
                         pageListClass (n, v) = n == "class" && v == "pagelist" 

hasAttr :: Eq str => str -> [Attribute str] -> Bool
hasAttr name = anyAttrName (== name)

fromAttrHref :: Tag L.ByteString -> L.ByteString
fromAttrHref = fromAttrib "href"

-------------------------

lbsToStrickBS :: L.ByteString -> BS.ByteString
lbsToStrickBS = BS.concat . L.toChunks

bsToS ::  BS.ByteString -> String
bsToS = T.unpack . T.decodeUtf8

lbsToS = bsToS . lbsToStrickBS
