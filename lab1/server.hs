import Network.Socket
import Control.Monad
import Control.Concurrent
import qualified Data.Map as Map
import Data.Maybe
import Data.List


main = do
    server <- socket AF_INET Stream defaultProtocol
    address <- inet_addr "127.0.0.1"
    bindSocket server $ SockAddrInet 4321 address
    listen server 5
    forever $ runServer server


runServer server = do
    conn <- accept server
    putStr "Connection accepted from "
    addrToStr (snd conn) >>= putStrLn
    let (SockAddrInet port host) = snd conn
    forkIO $ forever $ processConnection conn


addrToStr (SockAddrInet port host) = do
    h <- inet_ntoa host
    return $ h ++ ":" ++ show port


processConnection (client, clientAddr) = do
    message <- recv client 256
    case words message of
        []             -> return 0
        (command:args) -> do
            case Map.lookup command handlers of
                Nothing        -> send client "Command not found"
                (Just handler) -> handler args >>= send client


handlers = Map.fromList [
        ("add", adder),
        ("mul", multiplier),
        ("reverse", reverser),
        ("ping", ping),
        ("tellmeastory", tellmeastory)
    ]


adder args = return $ show $ sum $ map read args

multiplier args = return $ show $ product $ map read args

reverser args = return $ reverse (intercalate " " args)

ping _ = return "pong"

tellmeastory _ = putStr "waiting for input -> " >> getLine

