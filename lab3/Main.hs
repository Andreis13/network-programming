
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Arrow
import           Control.Concurrent
import           Control.Concurrent.STM.TChan
import           Control.Monad.STM
import           Data.Attoparsec.ByteString.Char8 hiding (parse, takeTill, skipWhile, Done, Fail)
import           Data.Attoparsec.ByteString.Lazy
import qualified Data.ByteString.Lazy.Char8 as BS
import qualified Data.Map as M
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
data SharedEvent = DrawPath { point::Point } | StartPath | Bye deriving Show


type Peers = M.Map UUID Path


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
    x <- double
    skipSpace
    y <- double
    return $ DrawPath (double2Float x, double2Float y)

serialize (DrawPath (x, y)) = BS.concat $ map BS.pack ["DRAWPATH ", show x, " ", show y]


main = withSocketsDo $ do
    net <- initNetwork
    playIO (InWindow "Lab3" (400, 400) (50, 50))
            white
            60
            (False, [], net)
            render
            handleInput
            update



render (_, paths, _) = return $ line paths


--handleInput (EventMotion _) w@(_, [], _) = return w
handleInput (EventMotion (x, y)) w@(keyPressed, _, net@(sendMsg, _)) = do
    if keyPressed
        then sendMsg $ DrawPath (x, y)
        else return 0
    return w

handleInput (EventKey key keyState mod (x, y)) w@(_, paths, net) = do
    return $ if keyState == Down then (True, paths, net) else (False, paths, net)

handleInput (EventResize (x, y)) w = do
    return w

update t w@(keyPressed, paths, net@(_, recvMsg)) = do
    let updatePaths = (\ps -> do
            maybeMsg <- recvMsg
            case maybeMsg of
                Nothing -> return ps
                Just m -> do
                    msg <- case parse message m of
                                Fail _ _ errMsg -> putStrLn errMsg >> error errMsg
                                Done _ s -> return s
                    let p = point $ what msg
                    updatePaths (p:ps))
    newPaths <- updatePaths paths
    return (keyPressed, newPaths, net)




initNetwork = do
    uuid <- nextRandom
    let uuidBytes = toByteString uuid

    sender <- socket AF_INET Datagram defaultProtocol
    setSocketOption sender ReuseAddr 1
    setSocketOption sender ReusePort 1
    setSocketOption sender Broadcast 1

    addrInfos <- getAddrInfo (Just defaultHints) (Just "255.255.255.255") (Just "3000")
    let addr = addrAddress $ head addrInfos
        sendMsg = (\msg -> do
            let s =( (uuidBytes `BS.append` (serialize msg)))
            --BS.putStrLn s
            sendTo sender (BS.toStrict s) addr)

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

    return (sendMsg, recvMsg)


