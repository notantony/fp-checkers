module Board
  ( Piece(..)
  , Board(..)
  ) where

import Data.Vector((!), (!?), Vector)
import Control.Monad(join)

data Color 
  = Black 
  | White
  deriving (Show, Eq)

data Piece 
  = Checker 
  | Qween
  deriving (Show, Eq)

newtype Field = Field { unField :: Maybe (Piece, Color) }
  deriving (Show, Eq)

newtype Board = Board { unBoard :: Vector (Field) }
  deriving (Show, Eq)

newtype Coord = Coord { unCoord :: (Int, Int) }
  deriving (Show, Eq)

getField :: Board -> Coord -> Maybe (Field)
getField (Board v) (Coord (x, y)) =
  if x >= 8 || x < 0 || y >= 8 || y < 0
  then Nothing
  else v !? (x + y * 8)

getFieldUnsafe :: Board -> Coord -> Field
getFieldUnsafe (Board v) (Coord (x, y)) =
  v ! (x + y * 8)

generateArea :: Coord -> Coord -> [Coord]
generateArea (Coord (x1, y1)) (Coord (x2, y2)) =
  filter (\(a, b) -> (a + b) `mod` 2 == 0) [ Coord (i, j) | i <- [x1, x1 + 1 .. x2], j <- [y1, y1 + 1 .. y2] ]

testColor :: Color -> Field -> Bool
testColor color (Maybe (piece, pieceColor)) = (color == pieceColor)

-- Coord must be valid
moveA :: Board -> Coord -> [Board]
moveA board (Coord (x, y))@c =
  let
    field = getFieldUnsafe board c
  in case 


-- succTake :: Color -> Board -> [Board]


-- succMove :: Color -> Board -> [Board]


-- succ :: Board -> [Board]

