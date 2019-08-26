module Engine
  ( runGame
  ) where

import Graphics.Gloss
import Graphics.Gloss.Data.Controller
import Graphics.Gloss.Interface.IO.Game
import System.Environment
import Control.Monad.Writer(execWriter, tell)

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

drawScene :: Scene -> IO Picture
drawScene (Scene actors) = do
  return $ Translate (-1080 / 2) (-860 / 2) $ Pictures $ map aPicture actors

handleEvent :: Event -> Scene -> IO Scene
handleEvent event x = do
  return x

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



decor :: Picture -> Actor
decor pic = Actor{ aId = "", aHandler = Nothing, aPicture = pic }
  
mainMenuScene :: Scene
mainMenuScene = Scene
  [ decor $ toBold $ makeTextLarge (-150, 200) "Main menu"
  , decor $ makeTextNormal (0, 0) "Play"
  , decor $ boardTex
  -- , decor $ whiteManTex
  -- , decor $ Translate 0 0 blackManTex
  , setupPiece (Coord (0, 7)) (Man White)
  , setupPiece (Coord (0, 6)) (Man Black)
  , setupPiece (Coord (1, 1)) (Man White)
  , setupPiece (Coord (2, 2)) (Man White)
  ]


--makeButton :: Handler -> Point -> String -> Actor
--makeButton (x, y) s =
windowSize :: (Int, Int)
windowSize = (1080, 860)
  
boardBegin :: (Float, Float)
boardBegin = (60, 60)

runGame :: IO ()
runGame =
  playIO
    (InWindow "Checkers" windowSize (0, 0))
    black
    1
    -- mainMenuScene
    (setupBoard startingPosition)
    drawScene
    handleEvent
    handleTick

coordToPos :: Coord -> (Float, Float)
coordToPos (Coord (x, y)) = (fromIntegral x * 100 + 60, fromIntegral y * 100 + 60)

setupPiece :: Coord -> Piece -> Actor
setupPiece coord piece = 
  decor $ (uncurry Translate) (coordToPos coord) (makePiece piece) 

setupBoard :: Board -> Scene
setupBoard board = Scene $ (decor $ boardTex) : pieces
  where
    dumped :: [(Coord, Piece)]
    dumped = dumpBoard board
    pieces :: [Actor]
    pieces = map (uncurry setupPiece) dumped