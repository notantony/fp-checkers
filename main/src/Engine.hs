module Engine
  ( runGame
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Point 
  ( pointInBox
  , Point(..)
  )
import qualified Graphics.Gloss.Data.Point.Arithmetic as PA 
  ( (+)
  , (-)
  , (*)
  , negate
  ) 
import System.Environment
import Control.Monad
  ( when
  )
import Data.Maybe
  ( catMaybes
  , fromMaybe
  )
import MyGraphics
  ( makeTextLarge
  , makeTextNormal
  , toBold
  , makePiece
  )
import Board
  ( Board(..)
  , Coord(..)
  , Piece(..)
  , Side(..)
  , Field(..)
  , Move(..)
  , dumpBoard
  , startingPosition
  , inBoardRange
  , getFieldUnsafe
  , makeMove
  , checkDir
  , getPiece
  , getPossibleMoves
  , tryMove
  , chainMoves
  )
import Resources
  ( boardTex
  , dotTex
  , ghostTex
  )
import Util
  ( runPipe
  , readNetworkCfg
  , fromIntegralPair
  , Serializable(..)
  , headMaybe
  )
import Control.Concurrent
  ( forkIO
  , MVar
  , putMVar
  , tryPutMVar
  , readMVar
  , tryTakeMVar
  , takeMVar
  )
import Server
  ( runClient
  )
import Data.List 
  ( sortBy
  )
import Data.IORef
  ( newIORef
  , IORef
  , readIORef
  , writeIORef
  , modifyIORef'
  )
import Network.Simple.TCP

newtype Scene = Scene { unScene :: [Actor] }

data Handler = Handler
  { hPrior  :: Int 
  , hAction :: Event -> Scene -> IO Scene
  }

data Actor = Actor
  { aId      :: String
  , aPicture :: Picture
  , aPrior   :: Int
  }

drawScene :: Scene -> IO Picture
drawScene (Scene actors) = do
  let (x, y) = PA.negate windowCenter
  return $ Translate x y $ Pictures $ map aPicture actors

handleEvent :: [Handler] -> Event -> Scene -> IO Scene
handleEvent handlers event scene@(Scene actors) = do
  let
    mx :: Int
    mx = maximum $ map hPrior handlers
    actions :: [Event -> Scene -> IO Scene]
    actions = map hAction $ filter (\handler -> hPrior handler == mx) handlers
    pipe :: [Scene -> IO Scene]
    pipe = map (\op -> op event) actions
  (Scene actors) <- runPipe pipe scene
  return $ setupScene actors

decorate :: String -> Picture -> Actor
decorate label pic = Actor{ aId = label, aPicture = pic, aPrior = 0 }

mainMenuScene :: Scene
mainMenuScene = Scene
  [ decorate "menu_txt" $ toBold $ makeTextLarge (-150, 200) "Main menu"
  , decorate "play_btn" $ makeTextNormal (0, 0) "Play"
  ]

loadingScreenScene :: Scene
loadingScreenScene = Scene
  [ decorate "loading_txt" $ makeTextNormal (400, 400) "Loading..."
  ]

windowSize :: Num a => (a, a)
windowSize = (860, 860)

windowCenter :: Point
windowCenter = 0.5 PA.* windowSize

boardBegin :: Num a => (a, a)
boardBegin = (60, 60)

fieldSide :: Num a => a
fieldSide = 100

runGame :: Side -> IO ()
runGame side = do
  (host, port) <- readNetworkCfg "client.cfg"
  (clientThreadId, sendBuf, recvBuf) <- runClient host port

  let
    register :: IO (Maybe String)
    register = do
      putMVar sendBuf (serialize side)
      ans <- takeMVar recvBuf
      return $ if ans == "ok"
      then Nothing
      else Just $ "Register failed: \"" ++ ans ++ "\""
    waitStart :: IO ()
    waitStart = do
      putStrLn "Waiting for other players"
      putMVar sendBuf "nop" 
      _ <- takeMVar recvBuf
      when (side == Black) $ putMVar sendBuf "nop"

  startPtr <- newIORef Nothing
  bodyPtr <- newIORef (Man side)
  queuePtr <- newIORef []
  boardPtr <- newIORef startingPosition
  sidePtr <- newIORef White

  let
    clearQueue :: IO ()
    clearQueue = do
      writeIORef startPtr Nothing
      writeIORef queuePtr []

    pushQueueAction :: Event -> Scene -> IO Scene
    pushQueueAction = makeFieldAction $ \coord scene@(Scene actors) -> do
      let
        setupPremove :: (Board, Coord) -> IO Scene
        setupPremove (board, coord) = do
          q <- readIORef queuePtr
          return $ nextScene coord (nextCoords $ headMaybe q)
          where
            nextCoords :: Maybe Move -> [Coord]
            nextCoords lastMoveM = map snd $ getPossibleMoves lastMoveM board coord
            nextScene :: Coord -> [Coord] -> Scene
            nextScene dotCoord nextCoords = Scene $ ghost : nextDots ++ filtered 
              where
                ghost :: Actor
                ghost = setupAtCoord ghostTex 1 "ghost" coord
                nextDots :: [Actor]
                nextDots = map (setupAtCoord dotTex 2 "ndot") nextCoords
                filtered :: [Actor]
                filtered = filter ((/=) "ndot" . aId) actors
      mStart <- readIORef startPtr
      case mStart of
        Nothing -> do
          board <- readIORef boardPtr
          case getPiece side coord board of
            Nothing -> return scene
            (Just body) -> do
              writeIORef startPtr $ Just coord
              writeIORef bodyPtr body
              setupPremove (board, coord)
        (Just start) -> do
          body <- readIORef bodyPtr
          board <- readIORef boardPtr
          q <- readIORef queuePtr
          let
            tryResult :: Maybe (Move, (Board, Coord))
            tryResult = do
              (_, lastCoord) <- chainMoves body q start board
              move <- makeMove coord lastCoord
              start <- mStart
              if checkDir move body
              then do
                result <- chainMoves body (move: q) start board
                return (move, result)
              else Nothing
          case tryResult of
            Nothing -> return scene
            Just (move, newPremove) -> do
              modifyIORef' queuePtr (\q -> move : q)
              setupPremove newPremove

    releaseQueueAction :: Event -> Scene -> IO Scene
    releaseQueueAction (EventKey (Char 'r') Down _ _) scene@(Scene actors) = do
      curSide <- readIORef sidePtr
      when (curSide == side) $ do
        mCoord <- readIORef startPtr
        case mCoord of 
          Nothing -> return ()
          (Just coord) -> do
            q <- readIORef queuePtr
            putMVar sendBuf $ serialize (q, coord)
            clearQueue
      return scene
    releaseQueueAction _ scene = return scene

    handleTick :: Float -> Scene -> IO Scene
    handleTick _ scene = do
      recieved <- tryTakeMVar recvBuf
      case recieved of
        Nothing -> return scene
        Just boardStr -> do
          let
            (newBoard, newSide) = deserialize boardStr :: (Board, Side)
          clearQueue
          writeIORef boardPtr newBoard
          when (newSide /= side) $ putMVar sendBuf "nop"
          writeIORef sidePtr newSide
          return $ makeBoard newBoard

  status <- register
  case status of
    (Just errorMsg) -> putStrLn errorMsg
    Nothing -> do
      waitStart
      playIO
        (InWindow "Checkers" windowSize (0, 0))
        black
        10
        -- boardScene
        (makeBoard startingPosition)
        drawScene
        ( handleEvent 
          [ Handler{ hAction = pushQueueAction, hPrior = 0 }
          , Handler{ hAction = releaseQueueAction, hPrior = 0 }
          ]
        )
        handleTick
  return ()

makeFieldAction :: (Coord -> Scene -> IO Scene) -> Event -> Scene -> IO Scene
makeFieldAction action (EventKey (MouseButton LeftButton) Down _ point) = do
  let
    normalized :: Point
    normalized = point PA.+ windowCenter
    mCoord :: Maybe Coord
    mCoord = pointToCoord normalized
  case mCoord of
    Nothing -> return
    Just coord -> action coord
makeFieldAction _ _ = return  

coordToPoint :: Coord -> Point
coordToPoint (Coord p) = fieldSide PA.* (fromIntegralPair p) PA.+ boardBegin

pointToCoord :: Point -> Maybe Coord
pointToCoord p = 
  if inBoardRange (a, b)
  then Just $ Coord (a, b)
  else Nothing
  where
    coord :: (Float, Float)
    coord = (1 / fieldSide) PA.* (p PA.- boardBegin)
    a :: Int
    a = floor $ fst coord
    b :: Int
    b = floor $ snd coord

setupPiece :: Piece -> Coord -> Actor
setupPiece piece = setupAtCoord (makePiece piece) 0 "piece"

setupAtCoord :: Picture -> Int -> String -> Coord -> Actor 
setupAtCoord tex prior label coord = Actor{aId = label, aPicture = placed, aPrior = prior}
  where
    placed :: Picture
    placed = uncurry Translate (coordToPoint coord) tex

setupScene :: [Actor] -> Scene
setupScene actors = Scene $ sortBy (\a b -> compare (aPrior a) (aPrior b)) actors

makeBoard :: Board -> Scene -- TODO: must-eat check
makeBoard board =
  let
    boardActor :: Actor
    boardActor = Actor{ aId = "board", aPicture = boardTex, aPrior = -1 }
    dumped :: [(Coord, Piece)]
    dumped = dumpBoard board
    pieces :: [Actor]
    pieces = map (uncurry . flip $ setupPiece) dumped
  in
    setupScene $ boardActor : pieces
