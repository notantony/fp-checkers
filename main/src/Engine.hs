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
  , getFieldUnsafe
  , makeMove
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

handleTick :: (MVar String, MVar String) -> Float -> Scene -> IO Scene
handleTick (sendBuf, recvBuf) _ x = do
  _ <- tryPutMVar sendBuf "hi"
  _ <- tryTakeMVar recvBuf
  return x

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
  
  coordPtr <- newIORef Nothing
  moveQueuePtr <- newIORef []
  boardPtr <- newIORef startingPosition

  let
    setCoord :: Maybe Coord -> IO ()
    setCoord = writeIORef coordPtr

    pushQueueAction :: Event -> Scene -> IO Scene
    pushQueueAction = makeFieldAction $ \coord scene@(Scene actors) -> do
      board <- readIORef boardPtr
      lastCoordM <- readIORef coordPtr
      let
        field :: Field 
        field = getFieldUnsafe board coord
        addDot :: Scene 
        addDot = Scene $ setupAtCoord dotTex 1 "dot" coord : actors
      case lastCoordM of
        Nothing -> case unField field of
          Nothing -> return scene
          (Just _) -> do
            setCoord $ Just coord
            return addDot
        (Just lastCoord) ->
          case makeMove coord lastCoord of
            Nothing -> return scene
            (Just move) -> do
              modifyIORef' moveQueuePtr (\q -> move : q)
              setCoord $ Just coord
              return addDot
              -- return $ Scene $ setupAtCoord dotTex "dot" coord : actors
    
    releaseQueueAction :: Event -> Scene -> IO Scene
    releaseQueueAction (EventKey (Char 'r') Down _ _) (Scene actors) = do
      -- q <- readIORef queuePtr
      setCoord Nothing
      writeIORef moveQueuePtr []
      return $ Scene $ filter (\actor -> aId actor /= "dot") actors
    releaseQueueAction _ scene = return scene

  playIO
    (InWindow "Checkers" windowSize (0, 0))
    black
    10
    -- boardScene
    (makeBoard startingPosition)
    drawScene
    (handleEvent 
      [ Handler{ hAction = pushQueueAction, hPrior = 0 }
      , Handler{ hAction = releaseQueueAction, hPrior = 0 }
      ]
    )
    (handleTick (sendBuf, recvBuf))
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

--makeButton :: Handler -> Point -> String -> Actor
--makeButton (x, y) s =