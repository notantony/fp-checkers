module Board
  ( Piece(..)
  , Board(..)
  , Color(..)
  , Coord(..)
  , Field(..)
  , dumpBoard
  , startingPosition
  ) where

import Data.Vector((!), (!?), Vector, fromList, (//))
import Data.Maybe
  ( isJust
  , mapMaybe
  )
import Control.Monad(join)
import Util
  ( pullMaybeSnd
  )

data Color 
  = Black 
  | White
  deriving (Show, Eq)

data Piece 
  = Man Color
  | King Color
  deriving (Show, Eq)

newtype Field = Field { unField :: Maybe Piece }
  deriving (Show, Eq)

newtype Board = Board { unBoard :: Vector (Field) }
  deriving (Show, Eq)

newtype Coord = Coord { unCoord :: (Int, Int) }
  deriving (Show, Eq)

instance Enum Coord where
  fromEnum c@(Coord (x, y)) =
    if x >= 8 || x < 0 || y >= 8 || y < 0
    then error $ "Bad coord: " ++ (show c)
    else x + y * 8
  toEnum num = 
    if num >= 64 || num < 0
    then error $ "Bad position: " ++ (show num)
    else Coord ((num `mod` 8), (num `div` 8))

getField :: Board -> Coord -> Maybe (Field)
getField (Board v) (Coord (x, y)) =
  if x >= 8 || x < 0 || y >= 8 || y < 0
  then Nothing
  else v !? (x + y * 8)

getFieldUnsafe :: Board -> Coord -> Field
getFieldUnsafe (Board v) c =
  v ! fromEnum c

generateArea :: Coord -> Coord -> [Coord]
generateArea (Coord (x1, y1)) (Coord (x2, y2)) =
  filter (\(Coord (a, b)) -> (a + b) `mod` 2 == 0) [ Coord (i, j) | i <- [x1, x1 + 1 .. x2], j <- [y1, y1 + 1 .. y2] ]

-- testColor :: Color -> Field -> Bool
-- testColor color (Maybe (piece, pieceColor)) = (color == pieceColor)

-- Coord must be valid
-- moveA :: Board -> Coord -> [Board]
-- moveA board (Coord (x, y))@c =
--   let
--     field = getFieldUnsafe board c
--   in case 

emptyBoard :: Board
emptyBoard = Board $ fromList (take 64 (repeat (Field Nothing)))

setPiece :: Piece -> Coord -> Board -> Board
setPiece piece coord =
  setPieces [(coord, piece)]

setPieces :: [(Coord, Piece)] -> Board -> Board
setPieces csps (Board v) =
  Board (v // (map (\(c, p) -> (fromEnum c, Field (Just p))) csps))

startingPosition :: Board
startingPosition = whitePieces `setPieces` (setPieces blackPieces emptyBoard)  
  where
    whitePieces :: [(Coord, Piece)]
    whitePieces = zip (generateArea (Coord (0, 0)) (Coord (7, 2))) (repeat (Man White))
    blackPieces :: [(Coord, Piece)]
    blackPieces = zip (generateArea (Coord (0, 5)) (Coord (7, 7))) (repeat (Man Black))

allArea :: [Coord]
allArea = generateArea (Coord (0, 0)) (Coord (7, 7))

takePopulated :: Board -> [Coord] -> [Coord]
takePopulated board coords = filter (\coord -> isJust $ unField $ getFieldUnsafe board coord) coords

dumpBoard :: Board -> [(Coord, Piece)]
dumpBoard board = mapMaybe pullMaybeSnd merged
  where
    merged :: [(Coord, Maybe Piece)]
    merged = map (\coord -> (coord, unField $ getFieldUnsafe board coord)) allArea
    

    
-- succTake :: Color -> Board -> [Board]

-- succMove :: Color -> Board -> [Board]

-- succ :: Board -> [Board]

