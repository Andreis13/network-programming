
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module Auderio
( initConvert
, getConvertStatus
, getDownloadLink
, ConvertStatus(..)
, DownloadLink(..)
)where

import Control.Applicative
import Control.Monad
import Data.Aeson
import Network.URI
import HTTP



data ConvertStatus = ConvertStatus {
    progress :: Maybe Float,
    convert_status   :: String
} deriving (Show)

data DownloadLink = DownloadLink {
    cloud_url :: String,
    donwload_status    :: String
} deriving (Show)

instance FromJSON ConvertStatus where
     parseJSON (Object v) = ConvertStatus <$>
                            v .:? "progress" <*>
                            v .: "status"
     parseJSON _          = mzero


instance FromJSON DownloadLink where
     parseJSON (Object v) = DownloadLink <$>
                            v .: "cloud_url" <*>
                            v .: "status"
     parseJSON _          = mzero



auderioUri path qstring = URI "http:" (Just auth) path qstring ""
    where auth = URIAuth "" "auderio.com" ""

jsonHeaders = [("Accept", "application/json"),
               ("Content-Type", "application/json")]

initConvert videoId = do
    let uri = auderioUri "/download" ("?url=https://www.youtube.com/watch?v=" ++ videoId)
    response <- get uri jsonHeaders
    case (code . status) response of
        200 -> return ()
        otherwise -> do
            print $ status response
            print $ headers response
            error "Could not initialize converison"


getConvertStatus videoId = do
    let uri = auderioUri "/check-download" ("?id=" ++ videoId)
    response <- get uri jsonHeaders
    (return . decode . body) response :: IO (Maybe ConvertStatus)



getDownloadLink videoId = do
    let uri = auderioUri "/get-cloud-link" ("?id=" ++ videoId)
    response <- get uri jsonHeaders
    (return . decode . body) response :: IO (Maybe DownloadLink)

