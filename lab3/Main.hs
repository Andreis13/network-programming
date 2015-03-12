
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Exception
import           Control.Monad
import           Control.Monad.STM
import           Data.Attoparsec.ByteString.Char8 hiding (parse, takeTill, takeWhile, skipWhile, Done, Fail)
import           Data.Attoparsec.ByteString.Lazy hiding (takeWhile)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4
import qualified Data.Set as S
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
--import           Graphics.Gloss.Internals.Interface.Game
--import           Graphics.Gloss.Internals.Interface.Backend
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString hiding (recv)
import           Network.Socket.ByteString.Lazy
import           Prelude hiding (take)
import           System.Exit



data Message = Message { who::UUID, what::SharedEvent } deriving Show
data SharedEvent = DrawPath { pathId::Int, point::Point } | VoteClear | Bye deriving Show

data Net a b = Net { sendMsg::a, recvMsg::b }

type Peers = M.Map UUID (Color, M.Map Int Path)


data World a = World { keyPressed :: Bool
                     , peers      :: Peers
                     , votes      :: S.Set UUID
                     , net        :: a
                     , idPool     :: [Int]
                     , colorPool  :: [Color]
                     }



nextId (x:xs) = (x, xs)


message = do
    uuidString <- take 16
    case fromByteString (BS.fromStrict uuidString) of
        Nothing -> fail "Could not parse UUID"
        Just uuid -> do
            evt <- sharedEvent
            return $ Message uuid evt

sharedEvent = do
    choice [drawPath, voteClear, bye]

drawPath = do
    string "DRAWPATH"
    skipSpace
    id <- decimal
    skipSpace
    x <- double
    skipSpace
    y <- double
    return $ DrawPath id (double2Float x, double2Float y)

bye = do
    string "BYE"
    return Bye

voteClear = do
    string "VOTECLEAR"
    return VoteClear

serialize (DrawPath id (x, y)) =
    BS.concat $ map BS.pack ["DRAWPATH ", show id, " ", show x, " ", show y]
serialize Bye = "BYE"
serialize VoteClear = "VOTECLEAR"


main = withSocketsDo $ do
    netFuncs <- initNetwork
    let handleExit = sendMsg netFuncs Bye >> return ()
    (playIO (InWindow "Lab3" (400, 400) (50, 50))
            white
            60
            (initialWorld netFuncs)
            render
            handleInput
            update') `finally` handleExit
    --(playWithBackendIO defaultBackendState
    --                  (InWindow "Lab3" (400, 400) (50, 50))
    --                  white
    --                  60
    --                  (World False initialPeers netFuncs [0..] initialColors)
    --                  render
    --                  handleInput
    --                  update'
    --                  True) `catch` handleExit


initialPeers = M.empty :: Peers

initialVotes = S.empty :: S.Set UUID

initialColors = cycle [black, red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]

initialWorld fs = World { keyPressed = False
                        , peers = initialPeers
                        , votes = initialVotes
                        , net = fs
                        , idPool = [0..]
                        , colorPool = initialColors }

render w = return $ pictures $ map drawPeer $ M.elems (peers w)
    where drawPeer = (\(c, paths) -> color c $ pictures $ map line $ M.elems paths)


--handleInput (EventMotion _) w@(_, [], _) = return w
handleInput (EventMotion (x, y)) w = do
    when (keyPressed w) $ void $ do
        sendMsg (net w) $ DrawPath (head $ idPool w) (x, y)
    return $ w

handleInput (EventKey key keyState mod (x, y)) w = do
    case key of
        SpecialKey KeyEsc -> do
            sendMsg (net w) Bye
            exitWith ExitSuccess
            return w
        SpecialKey KeySpace -> do
            sendMsg (net w) VoteClear
            return w
        MouseButton LeftButton -> do
            let newIds = tail $ idPool w
                newState = (keyState == Down)
            return $ w { keyPressed = newState, idPool = newIds }
        otherwise -> return w

handleInput (EventResize (x, y)) w = return w

--update t (World keyPressed peers' net i c) = do
--    let updatePeers = (\peers -> do
--            maybeMsg <- recvMsg net
--            case maybeMsg of
--                Nothing -> return peers
--                Just m -> do
--                    msg <- case parse message m of
--                                Fail _ _ errMsg -> putStrLn errMsg >> error errMsg
--                                Done _ s -> return s

--                    newPeers <- case what msg of
--                                    DrawPath pathId point -> do
--                                        return $ addPointToPeer point (who msg) peers
--                                    Bye -> do return $ removePeer (who msg) peers

--                    updatePeers newPeers)

update' t w = do
    let recvAll = do
            m <- recvMsg (net w)
            case m of
                Nothing -> return []
                Just msg -> recvAll >>= return . (msg:)

    messages <- map (\m -> case parse message m of
                                Fail _ _ errMsg -> error errMsg
                                Done _ s -> s) <$> recvAll


    return $ foldl (\w msg ->
                    let uuid = who msg
                    in  case what msg of
                            DrawPath pathId point ->
                                if uuid `M.member` (peers w)
                                    then w { peers = addPointToPeer point pathId uuid (peers w) }
                                    else w { peers = addPointToNewPeer point pathId uuid (head (colorPool w)) (peers w)
                                           , colorPool = tail (colorPool w) }
                            VoteClear ->
                                let newVotes = S.insert uuid (votes w)
                                in  if S.size newVotes * 2 > M.size (peers w)
                                        then w { peers = clearPeers (peers w)
                                               , votes = S.empty }
                                        else w { votes = newVotes}
                            Bye -> w { peers = removePeer uuid (peers w) }
                        ) w messages

addPointToNewPeer point pathId uuid color =
    M.insert uuid (color, M.singleton pathId [point])

addPointToPeer point pathId =
    M.adjust (\(color, paths) ->
        (color, M.insertWith' (\[p] ps -> p:ps) pathId [point] paths))

removePeer = M.delete

clearPeers = M.map (\(color, paths) -> (color, M.empty))


initNetwork = do
    uuid <- nextRandom
    let uuidBytes = toByteString uuid

    sock <- socket AF_INET Datagram defaultProtocol
    setSocketOption sock ReuseAddr 1
    setSocketOption sock ReusePort 1
    setSocketOption sock Broadcast 1
    bind sock (SockAddrInet 3000 iNADDR_ANY)

    addrInfos <- getAddrInfo (Just defaultHints) (Just "255.255.255.255") (Just "3000")
    let addr = addrAddress $ head addrInfos
        sendMsg = (\msg -> let bytes = uuidBytes `BS.append` (serialize msg)
                           in sendTo sock (BS.toStrict bytes) addr)

    incomingChan <- newTChanIO

    let receiver = do
            msg <- recv sock 1024
            atomically $ writeTChan incomingChan msg
            receiver

    forkIO receiver

    let recvMsg = atomically $ tryReadTChan incomingChan

    return $ Net sendMsg recvMsg


