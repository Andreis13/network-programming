

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PackageImports #-}

module HTTP
( get
, Status(..)
, Headers
, Response(..)
) where

import Control.Applicative
import Control.Exception
import Data.Monoid
import Data.Attoparsec.ByteString.Char8 hiding (parse, takeTill, skipWhile, Done, Fail)
import Data.Attoparsec.ByteString.Lazy

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.ByteString.Char8 as BSS
import Data.Maybe
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString.Lazy
import Network.URI
import OpenSSL
import qualified OpenSSL.Session as SSL
import "crypto-random" Crypto.Random
import Prelude hiding (take, takeWhile)
import System.IO ( IOMode(..) )



data Status = Status { code   :: Int
                       , reason :: BS.ByteString
                       } deriving Show

type Headers = [(BS.ByteString, BS.ByteString)]

data Response = Response { status  :: Status
                           , headers :: Headers
                           , body    :: BS.ByteString
                           } deriving Show



get (URI _ Nothing _ _ _) headers = error "No Host!"
get uri headers = withSocketsDo . withOpenSSL $ do
    let auth = fromJust $ uriAuthority uri
        host = uriRegName auth
        isSecure = uriScheme uri == "https:"
        port = if uriPort auth == ""
                    then if isSecure then "443" else "80"
                    else drop 1 $ uriPort auth -- remove ':'

        connectionHandler = if isSecure
                                then processRequestSocketSecure
                                else processRequestSocket

        defaultHeaders = [("Host", host), ("Connection", "close")]

        request = BS.pack ("GET " ++ uriPath uri ++ uriQuery uri ++ " HTTP/1.1\r\n")
                    `BS.append`
                    packHeaders (defaultHeaders ++ headers)
                    `BS.append`
                    "\r\n"


    contents <- connectionHandler host port request


    case parse httpResponse contents of
        Fail _ _ errMsg -> error errMsg
        Done _ s -> return s




openSocketStream host port = do
    addrInfos <- getAddrInfo (Just defaultHints) (Just host) (Just port)
    sock <- socket AF_INET Stream defaultProtocol
    connect sock $ addrAddress $ head addrInfos
    return sock

processRequestSocket host port request = do
    sock <- openSocketStream host port
    hSock <- socketToHandle sock ReadWriteMode
    BS.hPutStr hSock request
    BS.hGetContents hSock

processRequestSocketSecure host port request = do
    context <- SSL.context
    SSL.contextSetDefaultCiphers context
    sock <- openSocketStream host port
    ssl <- SSL.connection context sock
    SSL.connect ssl
    SSL.write ssl (BS.toStrict request)
    lazyRead ssl >>= (return . BS.fromChunks)

lazyRead ssl = do
    bs <- SSL.read ssl 2048 `catch` handler
    if B.null bs
        then return []
        else lazyRead ssl >>= (return . (bs:))
    where handler :: SSL.ConnectionAbruptlyTerminated -> IO BSS.ByteString
          handler e = return ""



packHeaders :: [(String, String)] -> BS.ByteString
packHeaders = BS.pack . (foldl (\hs (k, v) -> hs ++ k ++ ": " ++ v ++ "\r\n") "")


httpResponse = do
    status <- statusLine
    headers <- manyTill anyHeader endOfLine
    body <- case lookup "Transfer-Encoding" headers of
                Just "chunked" -> BS.concat <$> many' bodyChunk
                otherwise      -> integralbody
    return $ Response status headers body


statusLine = do
    string "HTTP/" >> digit >> char8 '.' >> digit -- HTTP version
    skipSpace
    code <- decimal -- status code
    skipSpace
    reason <- manyTill anyChar endOfLine
    return $ Status code (BS.pack reason)


anyHeader = do
    key <- manyTill anyChar (char8 ':')
    skipSpace
    val <- manyTill anyChar endOfLine
    return (BS.pack key, BS.pack val)


integralbody = takeLazyByteString


bodyChunk = do
    chunkSize <- hexadecimal
    skipWhile (not . isEndOfLine)
    endOfLine
    chunkContent <- take chunkSize
    skipSpace
    return $ BS.fromChunks [chunkContent]

