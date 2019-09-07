module Server
  ( sendMsg
  , localhost
  , defaultPort
  , runClient
  , runServer
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

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = do
  storage <- newIORef "wew"
  let
    welcomeReciever :: (Socket, SockAddr) -> IO ()
    welcomeReciever sockPair@(socket, sockAddr) = do
      putStrLn $ "Connected: " ++ (show sockPair)
      _ <- runBroadcast socket
      return ()
    sendStorage :: Socket -> IO ()
    sendStorage socket = do
      content <- readIORef storage
      sendMsg content socket
    runBroadcast :: Socket -> IO [ByteString]
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ (show socket) ++ " was closed" 
          return []
        Just msg -> do
          let unpacked = unpack msg
          putStrLn $ "Recieved: \"" ++ unpacked ++ "\" from " ++ (show socket) 
          if unpacked == "ping"
            then do
              sendStorage socket
              runBroadcast socket
            else do
              atomicModifyIORef' storage $ \_ -> (unpacked, ())
              sendStorage socket
              runBroadcast socket
  serve hostPref service welcomeReciever

localhost :: HostName
localhost = "127.0.0.1"

defaultPort :: ServiceName
defaultPort = "5050"

-- runServerDefault :: IO ()
-- runServerDefault = runServer (Host localhost) defaultPort

-- runClientDefault :: IO (MVar String, ThreadId)
-- runClientDefault = runClient localhost defaultPort

recvMsg :: Socket -> IO String
recvMsg socket = do
  buf <- recv socket 1024
  case buf of
    Nothing -> do
      putStrLn $ "Connection " ++ (show socket) ++ " was closed" 
      return []
    Just msg -> do
      let unpacked = unpack msg
      putStrLn $ "Recieved: \"" ++ unpacked ++ "\" from " ++ (show socket)  
      return unpacked

sendMsg :: String -> Socket -> IO ()
sendMsg msg socket = do
  putStrLn $ "Sending: \"" ++ msg ++ "\" to " ++ (show socket)
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
