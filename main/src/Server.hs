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
  , atomicModifyIORef'
  )
import Data.Foldable
  ( for_
  )
import Data.List
  ( elemIndex
  )
import Board
  ( startingPosition
  , Serializable(..)
  )

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = do
  boardPtr <- newIORef startingPosition
  players <- newIORef (Nothing, Nothing)
  let
    welcomeReciever :: (Socket, SockAddr) -> IO ()
    welcomeReciever sockPair@(socket, sockAddr) = do
      putStrLn $ "Connected: " ++ show sockPair
      _ <- runBroadcast socket
      return ()
    sendBoard :: Socket -> IO ()
    sendBoard socket = do
      board <- readIORef boardPtr
      sendMsg "ss" {- (serialize board) -} socket
    runBroadcast :: Socket -> IO [ByteString]
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ show socket ++ " was closed" 
          return []
        Just msg -> do
          let unpacked = unpack msg
          putStrLn $ "Recieved: \"" ++ unpacked ++ "\" from " ++ show socket 
          if unpacked == "ping"
            then do
              -- sendBoard socket
              sendMsg "pong" socket
              runBroadcast socket
            else do
              -- atomicModifyIORef' storage $ \_ -> (unpacked, ())
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
