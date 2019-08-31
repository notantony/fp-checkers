module Server
  ( runTranslator
  , runServerDefault
  , sendMsg
  , localhost
  , defaultPort
  , runTranslatorDefault
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
  )
import System.IO
import Data.ByteString.Char8
  ( ByteString
  , unpack
  , pack
  )
-- import Board
--   ( Ser)
  
recvAll :: Socket -> IO [ByteString]
recvAll socket = 
  do
    buf <- recv socket 1024
    case buf of
      Nothing -> do
        putStrLn $ "Connection " ++ (show socket) ++ " was closed" 
        return []
      Just msg -> do
        putStrLn $ "Recieved: \"" ++ (unpack msg) ++ "\""
        recvAll socket

-- sendBoard :: Board -> IO ()

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = serve hostPref service logAll
  where
    logAll :: (Socket, SockAddr) -> IO ()
    logAll (socket, sockAddr) = do
      putStrLn $ "Connected: " ++ show sockAddr ++ " / " ++ show socket
      buf <- recvAll socket
      putStrLn $ unpack $ mconcat buf

localhost :: HostName
localhost = "127.0.0.1"

defaultPort :: ServiceName
defaultPort = "5050"

runServerDefault :: IO ()
runServerDefault = runServer (Host localhost) defaultPort

runTranslatorDefault :: IO (MVar String, ThreadId)
runTranslatorDefault = runTranslator localhost defaultPort

sendMsg :: String -> (Socket, SockAddr) -> IO ()
sendMsg msg (socket, sockAddr) = do
  send socket $ pack msg

runTranslator :: HostName -> ServiceName -> IO (MVar String, ThreadId)
runTranslator host service = do
  buf <- newEmptyMVar
  let
    listenLoop :: (Socket, SockAddr) -> IO ()
    listenLoop sockPair = do
      msg <- takeMVar buf
      putStrLn $ "Sending: \"" ++ msg ++ "\""
      sendMsg msg sockPair
      listenLoop sockPair
  translatorThreadId <- forkIO $ connect host service listenLoop
  return (buf, translatorThreadId)

