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
import Data.IORef
  ( IORef
  , newIORef
  , atomicModifyIORef'
  , readIORef 
  )
import Data.Foldable
  ( for_
  )
  

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = do
  recievers <- newIORef []
  let
    welcomeReciever :: (Socket, SockAddr) -> IO ()
    welcomeReciever sockPair@(socket, sockAddr) = do
      putStrLn $ "Connected: " ++ (show sockPair)
      atomicModifyIORef' recievers (\xs -> ((sockPair : xs), ()))
      buf <- runBroadcast socket
      putStrLn $ unpack $ mconcat buf
    runBroadcast :: Socket -> IO [ByteString]
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ (show socket) ++ " was closed" 
          atomicModifyIORef' recievers (\xs -> ((filter (\(x, _) -> x == socket) xs), ()))
          return []
        Just msg -> do
          putStrLn $ "Recieved: \"" ++ (unpack msg) ++ "\""
          curRecievers <- readIORef recievers
          putStrLn $ "Broadcasting to: " ++ (show curRecievers)
          for_ curRecievers sendMsg "I'm broadcasting"
          runBroadcast socket
  serve hostPref service welcomeReciever

localhost :: HostName
localhost = "127.0.0.1"

defaultPort :: ServiceName
defaultPort = "5050"

runServerDefault :: IO ()
runServerDefault = runServer (Host localhost) defaultPort

runTranslatorDefault :: IO (MVar String, ThreadId)
runTranslatorDefault = runTranslator localhost defaultPort

sendMsg :: String -> Socket -> IO ()
sendMsg msg socket = do
  send socket $ pack msg

runTranslator :: HostName -> ServiceName -> IO (MVar String, ThreadId)
runTranslator host service = do
  listenBuf <- newEmptyMVar
  outputBuf <- newIORef [] 
  let
    listenLoop :: (Socket, SockAddr) -> IO ()
    listenLoop sockPair@(socket, _) = do
      msg <- takeMVar listenBuf
      putStrLn $ "Sending: \"" ++ msg ++ "\""
      sendMsg msg socket
      listenLoop sockPair
  translatorThreadId <- forkIO $ connect host service listenLoop
  return (listenBuf, translatorThreadId)
