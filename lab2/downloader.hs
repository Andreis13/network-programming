

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}


import           HTTP
import           Data.Maybe
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Time.Clock.POSIX
import           GHC.Generics
import           Network.URI

import           Auderio


-- mLgPNilXHM4


main = do
    let vId = "n5p9_p633xM"
    initConvert vId
    checkStatus vId
    maybeResp <- getDownloadLink vId

    let link = cloud_url (fromJust maybeResp)
    file <- get (fromJust (parseURI link)) []
    BS.writeFile "test.mp3" $ HTTP.body file




checkStatus videoId = do
    maybeResp <- getConvertStatus videoId
    let resp = fromJust maybeResp
        success = putStrLn "Converted" >> return ()
    case convert_status resp of
        "ready" -> success
        "downloaded" -> success
        otherwise -> putStrLn (convert_status resp ++ " -> " ++ (show . fromJust . progress) resp) >> checkStatus videoId


printHeaders = mapM_ (putStrLn . (\(k, v)-> BS.unpack k ++ ":\t" ++ BS.unpack v))

