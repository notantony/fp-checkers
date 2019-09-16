module Board
  ( Piece(..)
  , Board(..)
  , Side(..)
  , Coord(..)
  , Field(..)
  , dumpBoard
  , startingPosition
  , inBoardRange
  ) where

import Data.Vector
  ( (!)
  , (!?)
  , Vector
  , fromList
  , (//)
  , toList
  )
import Data.Maybe
  ( isJust
  , mapMaybe
  )
import Control.Monad(join)
import Util
  ( pullMaybeSnd
  )

data Side 
  = Black 
  | White
  deriving (Show, Eq)

data Piece 
  = Man Side
  | King Side
  deriving (Show, Eq)

newtype Field = Field { unField :: Maybe Piece }
  deriving (Show, Eq)

newtype Board = Board { unBoard :: Vector Field }
  deriving (Show, Eq)

newtype Coord = Coord { unCoord :: (Int, Int) }
  deriving (Show, Eq)

instance Enum Coord where
  fromEnum c@(Coord (x, y)) =
    if inBoardRange (x, y)
    then x + y * 8
    else error $ "Bad coord: " ++ (show c)
  toEnum num = 
    if num >= 64 || num < 0
    then error $ "Bad position: " ++ (show num)
    else Coord ((num `mod` 8), (num `div` 8))

inBoardRange :: (Int, Int) -> Bool
inBoardRange (x, y) = 
  x < 8 && x >= 0 && y < 8 && y >= 0

getField :: Board -> Coord -> Maybe Field
getField (Board v) (Coord (x, y)) =
  if inBoardRange (x, y)
  then v !? (x + y * 8)
  else Nothing

getFieldUnsafe :: Board -> Coord -> Field
getFieldUnsafe (Board v) c =
  v ! fromEnum c

generateArea :: Coord -> Coord -> [Coord]
generateArea (Coord (x1, y1)) (Coord (x2, y2)) =
  filter (\(Coord (a, b)) -> (a + b) `mod` 2 == 0) [ Coord (i, j) | i <- [x1, x1 + 1 .. x2], j <- [y1, y1 + 1 .. y2] ]

-- testSide :: Side -> Field -> Bool
-- testSide color (Maybe (piece, pieceSide)) = (color == pieceSide)

-- Coord must be valid
-- moveA :: Board -> Coord -> [Board]
-- moveA board (Coord (x, y))@c =
--   let
--     field = getFieldUnsafe board c
--   in case 

emptyBoard :: Board
emptyBoard = Board $ fromList $ replicate 64 (Field Nothing)

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
takePopulated board = filter (\coord -> isJust $ unField $ getFieldUnsafe board coord)

dumpBoard :: Board -> [(Coord, Piece)]
dumpBoard board = mapMaybe pullMaybeSnd merged
  where
    merged :: [(Coord, Maybe Piece)]
    merged = map (\coord -> (coord, unField $ getFieldUnsafe board coord)) allArea
    
serializeField :: Field -> Char
serializeField (Field Nothing)              = '0'
serializeField (Field (Just (Man Black)))   = '1'
serializeField (Field (Just (Man White)))   = '2'
serializeField (Field (Just (King Black)))  = '3'
serializeField (Field (Just (King Black)))  = '4'

serializeBoard :: Board -> String
serializeBoard (Board v) = map serializeField (toList v)

deserializeField :: Char -> Field
deserializeField '0' = Field Nothing
deserializeField '1' = Field $ Just $ Man Black
deserializeField '2' = Field $ Just $ Man White
deserializeField '3' = Field $ Just $ King Black
deserializeField '4' = Field $ Just $ King Black
deserializeField _   = error "Unxpected characted in serialized string" 

deserializeBoard :: String -> Board
deserializeBoard s = Board $ fromList $ map deserializeField s

-- succTake :: Side -> Board -> [Board]

-- succMove :: Side -> Board -> [Board]

-- succ :: Board -> [Board]

