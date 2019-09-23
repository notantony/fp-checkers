module Board
  ( Piece(..)
  , Board(..)
  , Side(..)
  , Coord(..)
  , Field(..)
  , dumpBoard
  , startingPosition
  , inBoardRange
  , getFieldUnsafe
  , makeMove
  , Serializable(..)
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
  , isNothing
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
    else Coord (num `mod` 8, num `div` 8)

getSide :: Piece -> Side
getSide (Man side) = side
getSide (King side) = side

sumCoord :: Coord -> Coord -> Coord
sumCoord (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 + x2, y1 + y2)

subCoord :: Coord -> Coord -> Coord
subCoord (Coord (x1, y1)) (Coord (x2, y2)) = Coord (x1 - x2, y1 - y2)

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

toField :: Piece -> Field
toField = Field . Just

emptyBoard :: Board
emptyBoard = Board $ fromList $ replicate 64 (Field Nothing)

setField :: Field -> Coord -> Board -> Board
setField field coord =
  setFields [(coord, field)]

setFields :: [(Coord, Field)] -> Board -> Board
setFields target (Board v) =
  Board $ v // (map (\(coord, field) -> (fromEnum coord, field)) target)

startingPosition :: Board
startingPosition = whitePieces `setFields` (setFields blackPieces emptyBoard)  
  where
    whitePieces :: [(Coord, Field)]
    whitePieces = zip (generateArea (Coord (0, 0)) (Coord (7, 2))) (repeat (toField $ Man White))
    blackPieces :: [(Coord, Field)]
    blackPieces = zip (generateArea (Coord (0, 5)) (Coord (7, 7))) (repeat (toField $ Man Black))

allArea :: [Coord]
allArea = generateArea (Coord (0, 0)) (Coord (7, 7))

takePopulated :: Board -> [Coord] -> [Coord]
takePopulated board = filter (isJust . unField . getFieldUnsafe board)

dumpBoard :: Board -> [(Coord, Piece)]
dumpBoard board = mapMaybe pullMaybeSnd merged
  where
    merged :: [(Coord, Maybe Piece)]
    merged = map (\coord -> (coord, unField $ getFieldUnsafe board coord)) allArea

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> a

instance Serializable Field where
  serialize (Field Nothing)              = "0"
  serialize (Field (Just (Man Black)))   = "1"
  serialize (Field (Just (Man White)))   = "2"
  serialize (Field (Just (King Black)))  = "3"
  serialize (Field (Just (King Black)))  = "4"
  
  deserialize "0" = Field Nothing
  deserialize "1" = toField $ Man Black
  deserialize "2" = toField $ Man White
  deserialize "3" = toField $ King Black
  deserialize "4" = toField $ King Black

instance (Serializable a) => Serializable [a] where
  serialize = show . (map serialize)
  deserialize = deserialize . read

instance Serializable Board where
  serialize = serialize . toList . unBoard
  deserialize = Board . fromList . deserialize

data Dir
  = NW
  | NE
  | SE
  | SW
  deriving (Show, Eq)

data Move 
  = Eat Dir
  | Walk Dir
  deriving (Show, Eq)

dirToCoord :: Dir -> Coord
dirToCoord NW = Coord (-1, 1)
dirToCoord NE = Coord (1, 1)
dirToCoord SE = Coord (1, -1)
dirToCoord SW = Coord (-1, -1)

coordToDir :: Coord -> Dir
coordToDir (Coord (-1, 1)) = NW 
coordToDir (Coord (1, 1)) = NE 
coordToDir (Coord (1, -1)) = SE 
coordToDir (Coord (-1, -1)) = SW 

getDir :: Move -> Dir
getDir (Eat dir) = dir
getDir (Walk dir) = dir

makeMove :: Coord -> Coord -> Maybe Move
makeMove a b
  | dist == 2 = Just $ Walk $ coordToDir diff
  | dist == 8 = Just $ Eat $ coordToDir $ Coord (x `div` 2, y `div` 2)
  | otherwise = Nothing
  where 
    diff :: Coord
    diff = a `subCoord` b
    x :: Int
    x = fst $ unCoord diff
    y :: Int
    y = snd $ unCoord diff
    dist :: Int
    dist = x ^ 2 + y ^ 2

-- TODO: "Turkish strike"?
tryMove :: Move -> Piece -> Coord -> Board -> Maybe (Board, Coord) -- Assuming side is correct
tryMove (Walk dir) piece coord board =
  case unField =<< getField board dst of
    Nothing -> Just (setFields [(coord, Field Nothing), (dst, toField piece)] board, dst)
    Just _ -> Nothing
  where
    dst :: Coord
    dst = coord `sumCoord` (dirToCoord dir)
    side :: Side
    side = getSide piece
tryMove (Eat dir) piece coord board =
  if isEmptyDst && isEnemyVictim
  then Just (setFields [(coord, Field Nothing), (victimCoord, Field Nothing), (dst, toField piece)] board, dst)
  else Nothing
  where
    dirCoord :: Coord
    dirCoord = dirToCoord dir
    victimCoord :: Coord
    victimCoord = coord `sumCoord` dirCoord
    dst :: Coord
    dst =  victimCoord `sumCoord` dirCoord
    isEmptyDst :: Bool
    isEmptyDst = isNothing $ unField =<< getField board dst
    isEnemyVictim :: Bool
    isEnemyVictim = 
      case unField =<< getField board victimCoord of
        Nothing -> False
        Just victim -> getSide victim /= getSide piece

manDir :: Side -> [Dir]
manDir Black = [NE, NW]
manDir White = [SE, SW]

checkMove :: Side -> Move -> Board -> Coord -> Bool
checkMove side move board coord = 
  case unField $ getFieldUnsafe board coord of
    Nothing -> False
    Just piece -> checkSide piece && testMove piece
  where
    checkSide :: Piece -> Bool 
    checkSide = ((==) side) . getSide
    testMove :: Piece -> Bool
    testMove (Man _ ) = elem (getDir move) (manDir side)
    testMove (King _) = True