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
import Text.HTML.TagSoup.Tree

main :: IO ()
main = do
    doc <- simpleHttp yURL 
    let allLinks = pageLinks (parsePageListUL doc)
        videoDiv = parseVideoDiv doc
        videoLinks = fetchVideoLink videoDiv
    mapM_ print videoLinks

--    mapM_ (print . lbsToS) allLinks
--    mapM_ print $ partitions isOpenForVideoLink videoDiv
--    mapM_ print videoLinks

fetchVideoLink xs = [x | x <- (universeTree $ tagTree xs), isVideoLink x ]
                    where isVideoLink (TagBranch n as _) = hasAttrValue' ("class","title") as
                          isVideoLink _ = False


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


---------------- //ul[class="pagelist"]/li/a[href]

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

-- | If Tag has a attr 
hasAttrTag name (TagOpen _ xs) = hasAttr name xs

hasAttr :: Eq str => str -> [Attribute str] -> Bool
hasAttr name = anyAttrName (== name)


-- | If Tag has a attr value
hasAttrValueTag attr (TagOpen _ xs) = hasAttrValue' attr xs

hasAttrValue' (name, value) attrs = anyAttr eqAttr attrs
                                    where eqAttr (n,v) = name == n && value == v
                 
fromAttrHref :: Tag L.ByteString -> L.ByteString
fromAttrHref = fromAttrib "href"


---------------- //div[class="right_video_nr01"]/ul/li/a[class="title"]

-- | //div[class="right_video_nr01"]

type Str = L.ByteString

isOpenFor :: Str -> (Str, Str) -> Tag L.ByteString -> Bool
isOpenFor name (attrName, attrValue) = tagOpen (== name) hasAttr
    where hasAttr = anyAttr (\ (n, v) -> attrName == n && attrValue == v )

isOpenForVideoDiv = isOpenFor "div" ("class", "right_video_nr01")
isCloseForVideoDiv = isTagCloseName "div"

parseOpenClose :: (Tag L.ByteString -> Bool)  -- Open For
                  -> (Tag L.ByteString -> Bool)  -- Close For      
                  -> [Tag L.ByteString]
                  -> [Tag L.ByteString]
parseOpenClose open close tags = takeWhile (not . close) $
                                dropWhile (not . open) tags

parseVideoDiv doc = parseOpenClose isOpenForVideoDiv isCloseForVideoDiv (parseTags doc)

-- | THIS IS WRONG
parseVideoLinks = getTagContent "a" 
                  (anyAttr (\ (n, v) -> n == "class" && v == "title"))

isOpenForVideoLink = isOpenFor "a" ("class", "title")

-------------------------

lbsToStrickBS :: L.ByteString -> BS.ByteString
lbsToStrickBS = BS.concat . L.toChunks

bsToS ::  BS.ByteString -> String
bsToS = T.unpack . T.decodeUtf8

lbsToS = bsToS . lbsToStrickBS
