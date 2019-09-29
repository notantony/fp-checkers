module Server
  ( sendMsg
  , runClient
  , runServer
  , startingPosition
  )
  where

import Network.Simple.TCP
import Control.Concurrent
  ( MVar
  , forkIO
  , newMVar
  , ThreadId
  , takeMVar
  , newEmptyMVar
  , putMVar
  )
import System.IO
import Data.ByteString.Char8
  ( ByteString
  , unpack
  , pack
  )
import Data.IORef
  ( IORef
  , newIORef
  , readIORef
  , writeIORef
  , atomicModifyIORef'
  , atomicWriteIORef
  )
import Data.Foldable
  ( for_
  )
import Data.List
  ( elemIndex
  )
import Board
  ( startingPosition
  , Board(..)
  , Side(..)
  , Move
  , chainMoves
  , getPiece
  , Coord(..)
  , switchSide
  )
import Util
  ( Serializable(..)
  , update
  )
import Text.Read
  ( readMaybe   
  )

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
    sendBoard :: Socket -> IO ()
    sendBoard socket = do
      board <- readIORef boardPtr
      sendMsg (serialize board) socket
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
                WaitStart -> 
                  case readMaybe unpacked :: Maybe Side of
                    Nothing -> do
                      sendMsg "Unexpected color" socket 
                      return ()
                    Just side -> do
                      let
                        fillSlot :: [Maybe Socket] -> ([Maybe Socket], Bool)
                        fillSlot arr = case slot of
                          Nothing -> (update id (Just socket) arr, True)
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
                        runBroadcast socket
                      else do
                        sendMsg "Slot is not empty" socket
                        return ()
                (WaitMove side) -> do
                  let
                    (moves, coord) = deserialize unpacked :: ([Move], Coord)
                  curBoard <- readIORef boardPtr
                  case getPiece side coord curBoard of
                    Nothing -> return ()
                    (Just piece) ->
                      case chainMoves piece moves coord curBoard of
                        Nothing -> return ()
                        (Just (board, _)) -> do
                          atomicWriteIORef statePtr (WaitMove $ switchSide side) 
                          writeIORef boardPtr board
                  sendBoard socket
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
      sendMsg msg socket
      response <- recvMsg socket
      putMVar recvBuf response
      listenLoop sockPair
  clientThreadId <- forkIO $ connect host service listenLoop
  return (clientThreadId, sendBuf, recvBuf)
