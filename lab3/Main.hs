
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Control.Applicative
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad
import           Control.Monad.STM
import           Data.Attoparsec.ByteString.Char8 hiding (parse, takeTill, takeWhile, skipWhile, Done, Fail)
import           Data.Attoparsec.ByteString.Lazy hiding (takeWhile)
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
import           Data.Maybe
import           Data.UUID
import           Data.UUID.V4
import           GHC.Float
import           Graphics.Gloss
import           Graphics.Gloss.Interface.IO.Game
import           Network.Socket hiding (send, sendTo, recv, recvFrom)
import           Network.Socket.ByteString hiding (recv)
import           Network.Socket.ByteString.Lazy
import           Prelude hiding (take)



data Message = Message { who::UUID, what::SharedEvent } deriving Show
data SharedEvent = DrawPath { pathId::Int, point::Point } | Bye deriving Show

data Net a b = Net { sendMsg::a, recvMsg::b }

type Peers = M.Map UUID (Color, M.Map Int Path)


data World a = World { keyPressed :: Bool
                     , peers      :: Peers
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
    drawPath

drawPath = do
    string "DRAWPATH"
    skipSpace
    id <- decimal
    skipSpace
    x <- double
    skipSpace
    y <- double
    return $ DrawPath id (double2Float x, double2Float y)

serialize (DrawPath id (x, y)) =
    BS.concat $ map BS.pack ["DRAWPATH ", show id, " ", show x, " ", show y]


main = withSocketsDo $ do
    netFuncs <- initNetwork
    playIO (InWindow "Lab3" (400, 400) (50, 50))
            white
            60
            (World False initialPeers netFuncs [0..] initialColors)
            render
            handleInput
            update'


initialPeers = M.empty :: Peers

initialColors = cycle [black, red, green, blue, yellow, cyan, magenta, rose, violet, azure, aquamarine, chartreuse, orange]


render w = return $ pictures $ map drawPeer $ M.elems (peers w)
    where drawPeer = (\(c, paths) -> color c $ pictures $ map line $ M.elems paths)


--handleInput (EventMotion _) w@(_, [], _) = return w
handleInput (EventMotion (x, y)) w@(World keyPressed peers net ids colors) = do
    when keyPressed $ void $ sendMsg net $ DrawPath (head ids) (x, y)
    return $ World keyPressed peers net ids colors

handleInput (EventKey key keyState mod (x, y)) (World _ peers net ids c) = do
    return $ World (keyState == Down) peers net (tail ids) c

handleInput (EventResize (x, y)) w = do
    return w

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

update' t (World keyPressed peers' net i c) = do
    let recvAll = do
            m <- recvMsg net
            case m of
                Nothing -> return []
                Just msg -> recvAll >>= return . (msg:)

    messages <- map (\m -> case parse message m of
                                Fail _ _ errMsg -> error errMsg
                                Done _ s -> s) <$> recvAll

    let (newPeers, newColors) =
            foldl (\(peers, colors) msg ->
                    let uuid = who msg
                    in  case what msg of
                            DrawPath pathId point ->
                                if uuid `M.member` peers
                                    then (addPointToPeer point pathId uuid peers, colors)
                                    else (addPointToNewPeer point pathId uuid (head colors) peers, tail colors)
                            Bye -> (removePeer uuid peers, colors)
                        ) (peers', c) messages

    return $ World keyPressed newPeers net i newColors

addPointToNewPeer point pathId uuid color =
    M.insert uuid (color, M.singleton pathId [point])

addPointToPeer point pathId =
    M.adjust (\(color, paths) ->
        (color, M.insertWith' (\[p] ps -> p:ps) pathId [point] paths))

removePeer = M.delete



initNetwork = do
    uuid <- nextRandom
    let uuidBytes = toByteString uuid

    sender <- socket AF_INET Datagram defaultProtocol
    setSocketOption sender ReuseAddr 1
    setSocketOption sender ReusePort 1
    setSocketOption sender Broadcast 1

    addrInfos <- getAddrInfo (Just defaultHints) (Just "255.255.255.255") (Just "3000")
    let addr = addrAddress $ head addrInfos
        sendMsg = (\msg -> let bytes = uuidBytes `BS.append` (serialize msg)
                           in sendTo sender (BS.toStrict bytes) addr)

    listener <- socket AF_INET Datagram defaultProtocol
    setSocketOption listener ReuseAddr 1
    setSocketOption listener ReusePort 1
    bind listener (SockAddrInet 3000 iNADDR_ANY)

    incomingChan <- newTChanIO

    let receiver = do
            msg <- recv listener 1024
            atomically $ writeTChan incomingChan msg
            receiver

    forkIO receiver

    let recvMsg = atomically $ tryReadTChan incomingChan

    return $ Net sendMsg recvMsg


