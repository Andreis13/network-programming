{-# LANGUAGE OverloadedStrings #-}

import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import qualified Data.ByteString.Char8 as BS

main = do
    sock <- socket AF_INET Stream defaultProtocol
    address <- inet_addr "127.0.0.1"
    connect sock $ SockAddrInet 4321 address
    repl sock
    sClose sock


repl sock = do
    s <- BS.getLine
    case BS.words s of
        []          -> repl sock
        ("close":_) -> sClose sock
        ("showtime":_) -> do
            send sock s
            response <- recv sock 65536
            BS.writeFile "t2.jpg" response
            putStrLn "File downloaded"
            repl sock
        otherwise -> do
            send sock s
            response <- recv sock 256
            BS.putStrLn response
            repl sock
