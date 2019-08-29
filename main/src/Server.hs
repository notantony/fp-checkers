module Server
  ( runClientDefault
  , runServerDefault
  )
  where

import Network.Simple.TCP
import Control.Concurrent
import System.IO
import Control.Concurrent.MVar
import Data.ByteString.Char8
  ( ByteString
  , unpack
  , pack
  )

recvAll :: Socket -> IO [ByteString]
recvAll socket = 
  do
    buf <- recv socket 1024
    case buf of
      Nothing -> return []
      Just msg -> do
        rest <- recvAll socket 
        return $ msg : rest

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = serve hostPref service logAll
  where
    logAll :: (Socket, SockAddr) -> IO ()
    logAll (socket, sockAddr) = do 
      putStrLn $ "TCP connection established from " ++ show sockAddr
      buf <- (recvAll socket)
      putStrLn $ unpack $ mconcat buf

localhost :: HostName
localhost = "127.0.0.1"

defaultPort :: ServiceName
defaultPort = "5050"

runServerDefault :: IO ()
runServerDefault = runServer (Host localhost) defaultPort

runClient :: HostName -> ServiceName -> IO () 
runClient host service = connect host service $ sendMsg "hello"

runClientDefault :: IO ()
runClientDefault = runClient localhost defaultPort

sendMsg :: String -> (Socket, SockAddr) -> IO ()
sendMsg msg (socket, sockAddr) = do
  send socket $ pack msg
  