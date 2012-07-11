{-# LANGUAGE OverloadedStrings #-}

module Main where

import Network.HTTP.Conduit
import Control.Monad.IO.Class (MonadIO)
import Control.Applicative ((<$>))
import Control.Monad
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import Data.Aeson
import Text.HTML.TagSoup

main :: IO ()
main = do
    re <- mapM fetchVideoItem [2000..2100]
    outputToFile $ encode re

fetchVideoItem :: MonadIO m => Int -> m VideoItem
fetchVideoItem i = let aid = show i
                   in liftM (VideoItem aid) (httpGetTitle (genURL aid))

-----------------------------------------------------

outputToFile :: L.ByteString -> IO ()
outputToFile = L.writeFile outputFileName

outputFileName :: FilePath
outputFileName = "liangxin.json"

baseURL :: String
baseURL = "http://liangxin.net.cn/plus/view.php?aid="

genURL :: String -> String
genURL i = baseURL ++ i


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
