{-# LANGUAGE OverloadedStrings #-}


import Network.Socket hiding (send, sendTo, recv, recvFrom)
import Network.Socket.ByteString
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString.Char8 as BS
import Data.Maybe
import Data.List
import Data.Time
import Data.Time.Format
import System.Locale


main = withSocketsDo $ do
    server <- socket AF_INET Stream defaultProtocol

    --if isSupportedSocketOption ReuseAddr
    --    then
    --    else return ()

    setSocketOption server ReuseAddr 0


    address <- inet_addr "127.0.0.1"
    bindSocket server $ SockAddrInet 4321 address
    listen server 5

    shutdownFlagMVar <- newMVar False
    connMVar         <- newEmptyMVar

    connectorTID <- forkIO $ acceptConnections server connMVar
    runServer server [] connMVar shutdownFlagMVar
    killThread connectorTID


acceptConnections server connMVar = do
    conn <- accept server
    putMVar connMVar conn
    acceptConnections server connMVar


runServer server connThreads newConnMVar shutdownFlagMVar = do
    shutdownFlag <- readMVar shutdownFlagMVar
    if shutdownFlag
        then do
            putStrLn $ show connThreads
            shutdownServer connThreads
        else do
            c <- tryTakeMVar newConnMVar
            case c of
                Nothing -> runServer server connThreads newConnMVar shutdownFlagMVar
                Just conn@(client, clientAddr) -> do
                    putStr "Connection accepted from "
                    addrToStr clientAddr >>= putStrLn
                    tid <- forkIO $ forever $ processConnection conn shutdownFlagMVar
                    putStrLn $ show tid
                    runServer server ((tid, conn):connThreads) newConnMVar shutdownFlagMVar


shutdownServer = mapM_ (\(tid, (sock, _)) -> shutdown sock ShutdownBoth >> killThread tid)



addrToStr (SockAddrInet port host) = do
    h <- inet_ntoa host
    return $ h ++ ":" ++ show port


processConnection (client, clientAddr) shutdownFlagMVar = do
    message <- recv client 256
    case BS.words message of
        []             -> return 0
        (command:args) -> do
            result <- case command of
                "add"          -> adder args
                "mul"          -> multiplier args
                "reverse"      -> reverser args
                "ping"         -> return "pong"
                "tellmeastory" -> tellmeastory
                "hastalavista" -> swapMVar shutdownFlagMVar True >> return ""
                "time"         -> getTime
                "showtime"     -> getT2
                otherwise      -> if BS.last command == '?'
                                    then return "42"
                                    else return "Can you elaborate on that?"
            send client result


getT2 = BS.readFile "Terminator2.jpg"

getTime = do
    tz <- getCurrentTimeZone
    utcTime <- getCurrentTime
    let localTime = utcToLocalTime tz utcTime
    return $ BS.pack $ formatTime defaultTimeLocale "%H:%M:%S" localTime

adder args = return $ BS.pack $ show $ sum $ map (read . BS.unpack) args

multiplier args = return $ BS.pack $ show $ product $ map (read . BS.unpack) args

reverser args = return $ BS.reverse (BS.intercalate " " args)

tellmeastory = putStr "waiting for input -> " >> BS.getLine

