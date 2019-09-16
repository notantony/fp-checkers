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
import Control.Monad.Writer(execWriter, tell)
import Data.Maybe
  ( catMaybes
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
  , dumpBoard
  , startingPosition
  , inBoardRange
  )
import Resources
  ( boardTex
  , dotTex
  )
import Util
  ( runPipe
  , readNetworkCfg
  , fromIntegralPair
  )
import Control.Concurrent
  ( forkIO
  , MVar
  , putMVar
  , tryPutMVar
  , readMVar
  , tryTakeMVar
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


handleTick :: (MVar String, MVar String) -> Float -> Scene -> IO Scene
handleTick (sendBuf, recvBuf) _ x = do
  _ <- tryPutMVar sendBuf "hi"
  _ <- tryTakeMVar recvBuf
  return x

-- drawScene :: Scene -> Picture
-- drawScene MainMenu = 
--   Pictures $ execWriter $ do
--   tell [toBold $ makeTextLarge (-100, 200) "Main menu"]
--   tell [makeTextNormal (0, 0) "Play"]
--   tell [t]
--   tell [makeTextNormal (1, 0) "Hello"]
--   tell [makeTextNormal (100, 100) "Hello"]

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

--makeButton :: Handler -> Point -> String -> Actor
--makeButton (x, y) s =
windowSize :: Num a => (a, a)
windowSize = (1080, 860)

windowCenter :: Point
windowCenter = 0.5 PA.* windowSize

boardBegin :: Num a => (a, a)
boardBegin = (60, 60)

fieldSide :: Num a => a
fieldSide = 100

runGame :: IO ()
runGame = do
  (host, port) <- readNetworkCfg "client.cfg"
  (clientThreadId, sendBuf, recvBuf) <- runClient host port

  
  -- aHandler = Just $ Handler 0 $ processQueue (boardPtr, queuePtr)
  -- aHandler = Just $ Handler 0 flashField

  -- boardPtr <- newIORef board
  -- queuePtr <- newIORef []

  -- putMVar sendBuf "hello"
  -- _ <- readMVar recvBuf
  -- putMVar buf "qqq"
  
  pathPtr <- newIORef []
  -- boardPtr <- newIORef startingPosition
  -- boardScene <- 

  playIO
    (InWindow "Checkers" windowSize (0, 0))
    black
    10
    (makeBoard startingPosition)
    -- boardScene
    -- loadingScreenScene
    drawScene
    (handleEvent []) -- Add handlers
    (handleTick (sendBuf, recvBuf))
  return ()

-- inArea :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
-- inArea (x1, y1) (x2, y2) (x, y) =
--   (x1 <= x && x <= x2) && (y1 <= y && y <= y2)

flashField :: Event -> Scene -> IO Scene
flashField (EventKey (MouseButton LeftButton) Down _ point) scene@(Scene actors) = do
  let
    normalized :: Point
    normalized = point PA.+ windowCenter
    mCoord :: Maybe Coord
    mCoord = pointToCoord normalized
  return $ case mCoord of
    Nothing -> scene
    Just coord -> Scene $ setupAtCoord dotTex "dot" coord : actors 
flashField _ scene = return scene

-- pushQueue :: Event -> Scene -> IO Scene
-- pushQueue (EventKey KeyBackspace Down _ _) =


-- processQueue :: (IORef Board, IORef [Coord]) -> Event -> Scene -> IO Scene
-- processQueue boardPtr (EventKey (Char 'r') Down _ _) (Scene actors) = do
--   board <- readIORef boardPtr
--   return $ Scene $ filter (\actor -> aId actor /= "dot") actors
-- processQueue boardPtr (EventKey (Char 's') Down _ _) (Scene actors) = do
--   board <- readIORef boardPtr
--   return $ Scene $ filter (\actor -> aId actor /= "dot") actors
-- processQueue _ _ scene = return scene

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
setupPiece piece = setupAtCoord (makePiece piece) "piece"

setupAtCoord :: Picture -> String -> Coord -> Actor 
setupAtCoord tex label coord = decorate label placed
  where
    placed :: Picture
    placed = (uncurry Translate) (coordToPoint coord) tex

setupScene :: [Actor] -> Scene
setupScene actors = Scene $ sortBy (\a b -> compare (aPrior a) (aPrior b)) actors

makeBoard :: Board -> Scene
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

-- hostGame :: Side -> IO () 
-- hostGame

-- joinGame :: MVar 

-- prepareGame :: MVar String -> Side -> IO ()

-- redrawBoard :: IORef Board -> IO Scene
-- redrawBoard boardPtr = do
--   board <- readIORef
--   return $ makeBoard board
