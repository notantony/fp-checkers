module Server
  ( sendMsg
  , runClient
  , runServer
  , startingPosition
  , dummyMsg
  , makeMsg
  , makeStrMsg
  , makeFinMsg
  , Message(..)
  )
  where

import Board (Board (..), Coord (..), Move, Side (..), allSides, chainMoves, getPiece, getStarts,
              promoteBoard, startingPosition, switchSide)
import Control.Concurrent (MVar, ThreadId, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (forM_, when)
import Data.ByteString.Char8 (pack, split, unpack)
import Data.IORef (atomicModifyIORef', atomicWriteIORef, newIORef, readIORef, writeIORef)
import Data.List (elemIndex)
import Data.Maybe (catMaybes, fromMaybe, isJust)
import Network.Simple.TCP (HostName, HostPreference, ServiceName, SockAddr, Socket, connect, recv,
                           send, serve)
import System.Exit (exitSuccess)
import Util (Serializable (..), update)

data Message = Message{ mHead :: String, mBody :: String }
  deriving (Eq)

instance Show Message where
  show Message{ mHead = hd, mBody = body } = hd ++ ": <" ++ body ++ ">"

instance Serializable Message where
  serialize Message{ mHead = hd, mBody = body } = "@" ++ hd ++ "@" ++ body
  deserialize string = Message { mHead = hd, mBody = body }
    where
      separatorPos :: Int
      separatorPos = fromMaybe (length string) (elemIndex '@' $ tail string)
      hd :: String
      hd = tail $ take (separatorPos + 1) string
      body :: String
      body = drop (separatorPos + 2) string

dummyMsg :: Message
dummyMsg = Message{ mHead = "none", mBody = "" }

data ServerState = WaitStart | WaitMove Side | EndGame

runServer :: HostPreference -> ServiceName -> IO ()
runServer hostPref service = do
  boardPtr <- newIORef startingPosition
  playersPtr <- newIORef [Nothing, Nothing]
  statePtr <- newIORef WaitStart
  let
    welcomeReciever :: (Socket, SockAddr) -> IO ()
    welcomeReciever sockPair@(socket, sockAddr) = do
      putStrLn $ "Connected: " ++ show sockPair ++ "\naddress: " ++ show sockAddr
      runBroadcast socket
      return ()
    broadcastBoard :: Side -> IO ()
    broadcastBoard side = do
      board <- readIORef boardPtr
      mPlayers <- readIORef playersPtr
      let
        players :: [Socket]
        players = catMaybes mPlayers
        canMove :: Side -> Bool
        canMove moveSide = not $ null $ getStarts moveSide board
        sendAll :: Message -> IO ()
        sendAll msg = mapM_ (sendMsg msg) players
      if all canMove allSides
      then sendAll $ makeMsg "board" (board, side)
      else do
        if not $ any canMove allSides
        then sendAll $ makeFinMsg "Draw"
        else
          forM_ (zip players allSides) $ \(socket, playerSide) ->
            if canMove playerSide
            then sendMsg (makeFinMsg "You win!!!") socket
            else sendMsg (makeFinMsg "You lose...") socket
        writeIORef statePtr EndGame
        exitSuccess

    runBroadcast :: Socket -> IO ()
    runBroadcast socket = do
      buf <- recv socket 1024
      case buf of
        Nothing -> do
          putStrLn $ "Connection " ++ show socket ++ " was closed"
          state <- readIORef statePtr
          case state of
            EndGame -> return ()
            _ -> do
              mPlayers <- readIORef playersPtr
              forM_ mPlayers $ \mPlayer ->
                case mPlayer of
                  Nothing -> return ()
                  Just playerSocket ->
                    when (playerSocket /= socket) $
                      sendMsg (makeFinMsg "Opponent has left the game.") playerSocket
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
                    Nothing  -> (update index (Just socket) arr, True)
                    (Just _) -> (arr, False)
                    where
                      index :: Int
                      index = fromEnum side
                      slot :: Maybe Socket
                      slot = arr !! index
                result <- atomicModifyIORef' playersPtr fillSlot
                if result
                then do
                  sendMsg (makeStrMsg "ok") socket
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
                    return $ promoteBoard board
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
            EndGame -> exitSuccess
  serve hostPref service welcomeReciever

recvMsg :: Socket -> IO [Message]
recvMsg socket = do
  buf <- recv socket 1024
  case buf of
    Nothing -> do
      putStrLn $ "Connection " ++ show socket ++ " was closed"
      return [makeStrMsg "Connection was closed"]
    Just bStr -> do
      let
        bites :: [String]
        bites = tail $ map unpack $ split '@' bStr
        merge2 :: [String] -> [Message]
        merge2 (hd : body : other) = deserialize ("@" ++ hd ++ "@" ++ body) : merge2 other
        merge2 _                   = []
        processMany :: [IO Message]
        processMany = flip map (merge2 bites) $ \msg -> do
          putStrLn $ "Recieved: \"" ++ show msg ++ "\" from " ++ show socket
          return msg
      sequence processMany

makeMsg :: (Serializable a) => String -> a -> Message
makeMsg hd obj = Message{ mHead = hd, mBody = serialize obj }

makeStrMsg :: String -> Message
makeStrMsg str = Message{ mHead = "str", mBody = str}

makeFinMsg :: String -> Message
makeFinMsg str = Message{ mHead = "finish", mBody = str}

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
      let
        sendIter :: IO ()
        sendIter = do
          msg <- takeMVar sendBuf
          if msg == dummyMsg -- TODO: finish
          then return ()
          else sendMsg msg socket
      sendIter
      (response : responses) <- recvMsg socket
      putMVar recvBuf response
      forM_ responses $ \msg -> do
        sendIter
        putMVar recvBuf msg
      listenLoop sockPair
  clientThreadId <- forkIO $ connect host service listenLoop
  return (clientThreadId, sendBuf, recvBuf)
