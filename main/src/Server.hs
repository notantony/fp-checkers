module Server
  ( sendMsg
  , runClient
  , runServer
  , startingPosition
  , dummyMsg
  , makeMsg
  , Message(..)
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

data Message = Message{ mHead :: String, mBody :: String }
  deriving (Eq)

instance Show Message where
  show Message{ mHead = head, mBody = body } = head ++ ": <" ++ body ++ ">"

instance Serializable Message where
  serialize Message{ mHead = head, mBody = body } = show (head, body)
  deserialize string = Message { mHead = head, mBody = body }
    where
      (head, body) = read string :: (String, String)

dummyMsg = Message{ mHead = "none", mBody = "" }

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
      mapM_ (sendMsg $ makeMsg "board" (board, side)) (catMaybes mPlayers)
    runBroadcast :: Socket -> IO ()
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ show socket ++ " was closed"
          return ()
        Just bStr -> do
          let
            msg :: Message
            msg = deserialize $ unpack bStr
          putStrLn $ "Recieved: \"" ++ show msg ++ "\" from " ++ show socket
          state <- readIORef statePtr
          case state of
            WaitStart ->
              if mHead msg /= "side"
              then sendMsg (makeStrMsg "Unexpected message, expected side") socket
              else do
                let
                  side :: Side
                  side = deserialize (mBody msg)
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
                  print "qwe"
                  sendMsg (makeStrMsg "ok") socket
                  print "qwe1"
                  players <- readIORef playersPtr
                  when (all isJust players) $ do
                    atomicWriteIORef statePtr $ WaitMove White
                    broadcastBoard White
                  runBroadcast socket
                else sendMsg (makeStrMsg "Slot is not empty") socket
            (WaitMove side) ->
              if mHead msg /= "move"
              then sendMsg (makeStrMsg "Slot is not empty") socket
              else do
                curBoard <- readIORef boardPtr
                let
                  (moves, coord) = deserialize (mBody msg) :: ([Move], Coord)
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

recvMsg :: Socket -> IO Message
recvMsg socket = do
  buf <- recv socket 1024
  case buf of
    Nothing -> do
      putStrLn $ "Connection " ++ show socket ++ " was closed"
      return $ makeMsg "none" "Connection was closed"
    Just bStr -> do
      let
        msg :: Message
        msg = deserialize $ unpack bStr
      putStrLn $ "Recieved: \"" ++ show msg ++ "\" from " ++ show socket
      return msg

makeMsg :: (Serializable a) => String -> a -> Message
makeMsg head obj = Message{ mHead = head, mBody = serialize obj }

makeStrMsg :: String -> Message
makeStrMsg str = Message{ mHead = "str", mBody = str}

sendMsg :: Message -> Socket -> IO ()
sendMsg msg socket = do
  putStrLn $ "Sending: \"" ++ show msg ++ "\" to " ++ show socket
  send socket $ pack $ serialize msg

runClient :: HostName -> ServiceName -> IO (ThreadId, MVar Message, MVar Message)
runClient host service = do
  sendBuf <- newEmptyMVar
  recvBuf <- newEmptyMVar
  let
    listenLoop :: (Socket, SockAddr) -> IO ()
    listenLoop sockPair@(socket, _) = do
      msg <- takeMVar sendBuf
      if msg == dummyMsg -- TODO: finish
      then return ()
      else sendMsg msg socket
      response <- recvMsg socket
      putMVar recvBuf response
      listenLoop sockPair
  clientThreadId <- forkIO $ connect host service listenLoop
  return (clientThreadId, sendBuf, recvBuf)
