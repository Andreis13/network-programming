

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

import Control.Arrow
import Data.Aeson
import Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import Network.URI
import HTTP

import GHC.Generics



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

data Y2MP3 = Y2MP3 {
    link :: String
} deriving (Show, Generic)

instance FromJSON Y2MP3



main :: IO ()
main = do
    let h = [("", "")
            ,("", "")]
    let uri = fromJust $ parseURI $ searchQuery "disturbed haunted"
    contents <- get uri []

    BS.writeFile "test.html" $ HTTP.body contents
    putStrLn $ show . BS.length $ HTTP.body contents
    putStrLn $ show $ HTTP.status contents
    printHeaders $ HTTP.headers contents


    let searchResponse = decode $ HTTP.body contents :: Maybe SearchResponse
        items = extractItems searchResponse

    displayOptions items

    selection <- getLine >>= return . read


    let videoId = fst $ items !! selection
        yt2mp3url = "http://youtubeinmp3.com/fetch/?api=advanced&format=JSON&video=http://www.youtube.com/watch?v="

        convertUri = fromJust $ parseURI $ yt2mp3url ++ videoId


    convertResponse <- get convertUri []
    print convertResponse

    let downloadInfo = decode $ HTTP.body convertResponse :: Maybe Y2MP3
        downloadUri = fromJust . parseURI . link . fromJust $ downloadInfo

    file <- get downloadUri []
    BS.writeFile "test.mp3" $ HTTP.body file
    putStrLn $ show $ HTTP.status contents
    printHeaders $ HTTP.headers file


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
