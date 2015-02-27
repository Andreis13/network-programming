

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import           Auderio
import           Control.Arrow
import           Data.Aeson
import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import           Network.URI
import           HTTP

import           GHC.Generics



data ItemId = ItemId {
    videoId :: String
} deriving (Show, Generic)

instance FromJSON ItemId

data ItemSnippet = ItemSnippet {
    title :: String
} deriving (Show, Generic)

instance FromJSON ItemSnippet

data Item = Item {
    id :: ItemId,
    snippet :: ItemSnippet
} deriving (Show, Generic)

instance FromJSON Item

data SearchResponse = SearchResponse {
    items :: [Item]
} deriving (Show, Generic)

instance FromJSON SearchResponse



main :: IO ()
main = do

    putStrLn "Enter the title of a song:"
    sq <- getLine

    let uri = fromJust $ parseURI $ searchQuery sq
    contents <- get uri []

    let searchResponse = decode $ HTTP.body contents :: Maybe SearchResponse
        items = extractItems searchResponse

    displayOptions items
    putStrLn "Choose one variant to download as mp3"
    selection <- getLine >>= return . read

    let (videoId, videoTitle) = items !! selection

    initConvert videoId
    checkStatus videoId
    maybeResp <- getDownloadLink videoId

    let link = cloud_url (fromJust maybeResp)
    file <- get (fromJust (parseURI link)) []
    BS.writeFile (videoTitle ++ ".mp3") $ HTTP.body file
    putStrLn "Downloaded!"



checkStatus videoId = do
    maybeResp <- getConvertStatus videoId
    let resp = fromJust maybeResp
        success = putStrLn "Converted" >> return ()
    case convert_status resp of
        "ready" -> success
        "downloaded" -> success
        otherwise -> putStrLn (convert_status resp ++ " -> " ++ (show . fromJust . progress) resp) >> checkStatus videoId


searchQuery searchString = path ++ qstring ++ key
    where
        path = "https://www.googleapis.com/youtube/v3/search"
        qstring = "?part=snippet&type=video&q=" ++ map replaceSpace searchString
        key = "&key=" ++ googleApiKey
        googleApiKey = "AIzaSyD9YBhamaGdbQSEhsIfT3CMCPMR9SJuaWs" :: String
        replaceSpace ' ' = '+'
        replaceSpace a = a

extractItems Nothing = []
extractItems (Just searchResponse) = map (getId &&& getTitle) $ items searchResponse
    where getId = Main.id >>> videoId
          getTitle = snippet >>> title

displayOptions = mapM_ (\(i, (_, title)) -> putStrLn (show i ++ ". " ++ title)) . zip [0..]

printHeaders = mapM_ (putStrLn . (\(k, v)-> BS.unpack k ++ ":\t" ++ BS.unpack v))
