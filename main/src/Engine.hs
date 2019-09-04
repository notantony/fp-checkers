module Engine
  ( runGame
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Interface.IO.Game
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
  , Color(..)
  , Field(..)
  , dumpBoard
  , startingPosition
  )
import Resources
  ( boardTex
  , blackManTex
  , whiteManTex
  )
import Util
  ( runPipe
  )
import Control.Concurrent
  ( forkIO
  , MVar
  )
import Server
  ( runTranslatorDefault    
  )

newtype Scene = Scene { unScene :: [Actor] }

data Handler = Handler 
  { hPrior  :: Int 
  , hAction :: Event -> Scene -> IO Scene
  }

data Actor = Actor
  { aId      :: String
  , aHandler :: Maybe Handler
  , aPicture :: Picture
  }


--   Scene -> (IO Sce)

--   ((Scene -> IO Scene) -> Scene -> Scene) -> Scene -> [Scene -> IO Scene] -> Scene
--   (Scene -> (Scene -> IO Scene) -> Scene) -> Scene -> [Scene -> IO Scene] -> Scene

-- (a -> IO a) -> (IO a -> IO a)


drawScene :: Scene -> IO Picture
drawScene (Scene actors) = do
  return $ Translate (-1080 / 2) (-860 / 2) $ Pictures $ map aPicture actors

handleEvent :: Event -> Scene -> IO Scene
handleEvent event scene@(Scene actors) = 
  let
    handlers :: [Handler]
    handlers = catMaybes $ map aHandler actors
    mx :: Int
    mx = maximum $ map hPrior handlers
    actions :: [Event -> Scene -> IO Scene]
    actions = map hAction $ filter (\handler -> hPrior handler == mx) handlers
    pipe :: [Scene -> IO Scene]
    pipe = map (\op -> op event) actions
  in
    (runPipe pipe) scene

handleTick :: Float -> Scene -> IO Scene
handleTick _ x = do
  return x

-- drawScene :: Scene -> Picture 
-- drawScene MainMenu = 
  -- Pictures $ execWriter $ do
  -- tell [toBold $ makeTextLarge (-100, 200) "Main menu"]
  -- tell [makeTextNormal (0, 0) "Play"]
  -- tell [t]
  -- tell [makeTextNormal (1, 0) "Hello"]
  -- tell [makeTextNormal (100, 100) "Hello"]


decorate :: Picture -> Actor
decorate pic = Actor{ aId = "", aHandler = Nothing, aPicture = pic }
  
mainMenuScene :: Scene
mainMenuScene = Scene
  [ decorate $ toBold $ makeTextLarge (-150, 200) "Main menu"
  , decorate $ makeTextNormal (0, 0) "Play"
  ]

loadingScreenScene :: Scene
loadingScreenScene = Scene
  [ decorate $ makeTextNormal (400, 400) "Loading..."
  ]

--makeButton :: Handler -> Point -> String -> Actor
--makeButton (x, y) s =
windowSize :: (Int, Int)
windowSize = (1080, 860)
  
boardBegin :: (Float, Float)
boardBegin = (60, 60)

runGame :: IO ()
runGame = do
  -- (buf, listenerId) <- runTranslatorDefault

  playIO
    (InWindow "Checkers" windowSize (0, 0))
    black
    1
    -- (setupBoard startingPosition)
    loadingScreenScene
    drawScene
    handleEvent
    handleTick
  return ()

inArea :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Bool
inArea (x1, y1) (x2, y2) (x, y) = (x1 <= x && x <= x2) && (y1 <= y && y <= y2)

flashField :: Coord -> Event -> Scene -> IO Scene
flashField (Coord (a, b)) (EventKey (MouseButton _) _ _ (x, y)) _ = return $ Scene []
flashField (Coord (a, b)) _ scene = return scene

coordToPos :: Coord -> (Float, Float)
coordToPos (Coord (a, b)) = (fromIntegral a * 100 + 60, fromIntegral b * 100 + 60)

setupPiece :: Coord -> Piece -> Actor
setupPiece coord piece =
  Actor{ aId = "piece", aHandler = Just handler, aPicture = pic }
    where 
      pic :: Picture
      pic = (uncurry Translate) (coordToPos coord) (makePiece piece) 
      handler :: Handler
      handler =  Handler 0 $ flashField coord

setupBoard :: Board -> Scene
setupBoard board = Scene $ (decorate $ boardTex) : pieces
  where
    dumped :: [(Coord, Piece)]
    dumped = dumpBoard board
    pieces :: [Actor]
    pieces = map (uncurry setupPiece) dumped