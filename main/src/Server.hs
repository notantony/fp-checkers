module Server
  ( sendMsg
  , runClient
  , runServer
  , startingPosition
  )
  where

import Board (Board (..), Coord (..), Move, Side (..), chainMoves, finalize, getPiece,
              startingPosition, switchSide)
import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Monad (when)
import Data.ByteString.Char8 (ByteString, pack, unpack)
import Data.Foldable (for_)
import Data.IORef (IORef, atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import Data.Maybe (catMaybes, isJust)
import Network.Simple.TCP
import System.IO
import Text.Read (readMaybe)
import Util (Serializable (..), update)

-- Shalyto?
data ServerState = WaitStart | WaitMove Side

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = do
  boardPtr <- newIORef startingPosition
  playersPtr <- newIORef [Nothing, Nothing]
  statePtr <- newIORef WaitStart
  let
    welcomeReciever :: (Socket, SockAddr) -> IO ()
    welcomeReciever sockPair@(socket, sockAddr) = do
      putStrLn $ "Connected: " ++ show sockPair
      runBroadcast socket
      return ()
    broadcastBoard :: Side -> IO ()
    broadcastBoard side = do
      board <- readIORef boardPtr
      mPlayers <- readIORef playersPtr
      mapM_ (sendMsg $ serialize (board, side)) (catMaybes mPlayers)
    runBroadcast :: Socket -> IO ()
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ show socket ++ " was closed"
          return ()
        Just msg -> do
          let
            unpacked :: String
            unpacked = unpack msg
          putStrLn $ "Recieved: \"" ++ unpacked ++ "\" from " ++ show socket
          if unpacked == "ping"
            then do
              sendMsg "pong" socket
              runBroadcast socket
            else do
              state <- readIORef statePtr
              case state of
                WaitStart -> do
                  let
                    side :: Side
                    side = deserialize unpacked
                    fillSlot :: [Maybe Socket] -> ([Maybe Socket], Bool)
                    fillSlot arr = case slot of
                      Nothing  -> (update id (Just socket) arr, True)
                      (Just _) -> (arr, False)
                      where
                        id :: Int
                        id = fromEnum side
                        slot :: Maybe Socket
                        slot = arr !! id
                  result <- atomicModifyIORef' playersPtr fillSlot
                  if result
                  then do
                    sendMsg "ok" socket
                    players <- readIORef playersPtr
                    when (all isJust players) $ do
                      atomicWriteIORef statePtr $ WaitMove White
                      broadcastBoard White
                    runBroadcast socket
                  else sendMsg "Slot is not empty" socket
                (WaitMove side) -> do
                  curBoard <- readIORef boardPtr
                  let
                    (moves, coord) = deserialize unpacked :: ([Move], Coord)
                    mNewBoard :: Maybe Board
                    mNewBoard = do
                      when (null moves) Nothing
                      piece <- getPiece side coord curBoard
                      (board, _) <- chainMoves piece moves coord curBoard
                      return $ finalize board
                  case mNewBoard of
                    Nothing -> broadcastBoard side
                    (Just board) -> do
                      let
                        newSide :: Side
                        newSide = switchSide side
                      atomicWriteIORef statePtr (WaitMove $ switchSide side)
                      writeIORef boardPtr board
                      broadcastBoard newSide
              runBroadcast socket
  serve hostPref service welcomeReciever

recvMsg :: Socket -> IO String
recvMsg socket = do
  buf <- recv socket 1024
  case buf of
    Nothing -> do
      putStrLn $ "Connection " ++ show socket ++ " was closed"
      return []
    Just msg -> do
      let unpacked = unpack msg
      putStrLn $ "Recieved: \"" ++ unpacked ++ "\" from " ++ show socket
      return unpacked

sendMsg :: String -> Socket -> IO ()
sendMsg msg socket = do
  putStrLn $ "Sending: \"" ++ msg ++ "\" to " ++ show socket
  send socket $ pack msg

runClient :: HostName -> ServiceName -> IO (ThreadId, MVar String, MVar String)
runClient host service = do
  sendBuf <- newEmptyMVar
  recvBuf <- newEmptyMVar
  let
    listenLoop :: (Socket, SockAddr) -> IO ()
    listenLoop sockPair@(socket, _) = do
      msg <- takeMVar sendBuf
      if msg == "nop"
      then return ()
      else sendMsg msg socket
      response <- recvMsg socket
      putMVar recvBuf response
      listenLoop sockPair
  clientThreadId <- forkIO $ connect host service listenLoop
  return (clientThreadId, sendBuf, recvBuf)
