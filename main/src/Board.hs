module Board
  ( Piece(..)
  , Board(..)
  , Side(..)
  , Coord(..)
  , Field(..)
  , Move(..)
  , dumpBoard
  , startingPosition
  , inBoardRange
  , getFieldUnsafe
  , makeMove
  , chainMoves
  -- , checkMove
  , checkDir
  , getPiece
  , switchSide
  , getPossibleMoves
  , tryMove
  , finalize
  ) where

import Control.Monad (join)
import Data.Maybe (isJust, isNothing, mapMaybe)
import Data.Vector (Vector, fromList, toList, (!), (!?), (//))
import MyTH (mkConv, mkShow)
import Util (Marge (..), Serializable (..), makeSerialization, pullMaybeSnd)

data Side
  = Black
  | White
  deriving (Show, Eq)

sideSerialization :: (Side -> String, String -> Side)
sideSerialization = makeSerialization
  [ (White, "w")
  , (Black, "b")
  ]

instance Serializable Side where
  serialize = fst sideSerialization
  deserialize = snd sideSerialization

instance Enum Side where
  fromEnum White = 0
  fromEnum Black = 1
  toEnum 0 = White
  toEnum 1 = Black

switchSide :: Side -> Side
switchSide Black = White
switchSide White = Black

data Piece
  = Man Side
  | King Side
  deriving (Show, Eq)

newtype Field = Field { unField :: Maybe Piece }
  deriving (Show, Eq)

fieldSerialization :: (Field -> String, String -> Field)
fieldSerialization = makeSerialization
  [ (Field Nothing, "_")
  , (Field (Just (Man Black)), "b")
  , (Field (Just (Man White)), "w")
  , (Field (Just (King Black)), "B")
  , (Field (Just (King White)), "W")
  ]

promote :: Piece -> Piece
promote (Man color)  = King color
promote (King color) = King color

instance Serializable Field where
  serialize = fst fieldSerialization
  deserialize = snd fieldSerialization

newtype Board = Board { unBoard :: Vector Field }
  deriving (Show, Eq)

instance Serializable Board where
  serialize = serialize . toList . unBoard
  deserialize = Board . fromList . deserialize

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

instance Serializable Coord where
  serialize = show . fromEnum
  deserialize = toEnum . read

getSide :: Piece -> Side
getSide (Man side)  = side
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

getPiece :: Side -> Coord -> Board -> Maybe Piece
getPiece side coord board = do
  piece <- unField $ getFieldUnsafe board coord
  if getSide piece == side
    then Just piece
    else Nothing

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

data Dir
  = NW
  | NE
  | SE
  | SW
  deriving (Show, Eq)

dirSerializarion :: (Dir -> String, String -> Dir)
dirSerializarion = makeSerialization
  [ (NW, "0")
  , (NE, "1")
  , (SW, "2")
  , (SE, "3")
  ]

instance Serializable Dir where
  serialize = fst dirSerializarion
  deserialize = snd dirSerializarion

data Move
  = Eat Dir
  | Walk Dir
  deriving (Show, Eq)

instance Serializable Move where
  serialize (Walk dir) = 'w' : serialize dir
  serialize (Eat dir)  = 'e' : serialize dir
  deserialize str@(char : chars)
    | char == 'w' = Walk $ deserialize chars
    | char == 'e' = Eat $ deserialize chars
    | otherwise = error $ "Cannot deserialize Walk from" ++ str -- TODO: TH?
  deserialize _ = error "Cannot deserialize Walk from empty string"

dirToCoord :: Dir -> Coord
dirToCoord NW = Coord (-1, 1)
dirToCoord NE = Coord (1, 1)
dirToCoord SE = Coord (1, -1)
dirToCoord SW = Coord (-1, -1)

coordToDir :: Coord -> Dir
coordToDir (Coord (-1, 1))  = NW
coordToDir (Coord (1, 1))   = NE
coordToDir (Coord (1, -1))  = SE
coordToDir (Coord (-1, -1)) = SW

getDir :: Move -> Dir
getDir (Eat dir)  = dir
getDir (Walk dir) = dir

pieceDirs :: Piece -> [Dir]
pieceDirs (Man side) = manDir side
  where
    manDir :: Side -> [Dir]
    manDir White = [NE, NW]
    manDir Black = [SE, SW]
pieceDirs (King _) = allDir
  where
    allDir :: [Dir]
    allDir = [NE, NW, SE, SW]

checkDir :: Move -> Piece -> Bool
checkDir move piece = elem (getDir move) (pieceDirs piece)
checkDir _ (King _) = True

makeMove :: Coord -> Coord -> Maybe Move
makeMove a b
  | dist == 2 = Just $ Walk $ coordToDir diff
  | dist == 8 = Just $ Eat $ coordToDir $ Coord (x `div` 2, y `div` 2)
  | otherwise = Nothing
  where
    diff :: Coord
    diff = a `subCoord` b
    (x, y) = unCoord diff :: (Int, Int)
    dist :: Int
    dist = x ^ 2 + y ^ 2

-- TODO: "Turkish strike"?
tryMove :: Piece -> Move -> Coord -> Board -> Maybe (Board, Coord) -- Assuming side is correct
tryMove piece (Walk dir) coord board =
  case getField board dst of
    Nothing -> Nothing
    Just (Field mPiece) -> case mPiece of
      Nothing  -> Just (setFields [(coord, Field Nothing), (dst, toField piece)] board, dst)
      (Just _) -> Nothing
  where
    dst :: Coord
    dst = coord `sumCoord` dirToCoord dir
    side :: Side
    side = getSide piece
tryMove piece (Eat dir) coord board =
  if isEmptyDst && isEnemyVictim
  then Just (setFields [(coord, Field Nothing), (victimCoord, Field Nothing), (dst, toField piece)] board, dst)
  else Nothing
  where
    dirCoord :: Coord
    dirCoord = dirToCoord dir
    victimCoord :: Coord
    victimCoord = coord `sumCoord` dirCoord
    dst :: Coord
    dst = victimCoord `sumCoord` dirCoord
    isEmptyDst :: Bool
    isEmptyDst =
      case getField board dst of
        Nothing -> False
        Just a  -> isNothing $ unField a
    isEnemyVictim :: Bool
    isEnemyVictim =
      case unField =<< getField board victimCoord of
        Nothing     -> False
        Just victim -> getSide victim /= getSide piece

chainMoves :: Piece -> [Move] -> Coord -> Board -> Maybe (Board, Coord)
chainMoves piece moves coord board =
  if checkEat
  then foldr foldSingle (Just (board, coord)) moves
  else Nothing
  where
    isEat :: Move -> Bool
    isEat (Eat _) = True
    isEat _       = False
    checkEat :: Bool
    checkEat = all isEat moves || length moves == 1
    foldSingle :: Move -> Maybe (Board, Coord) -> Maybe (Board, Coord)
    foldSingle move mPos = do
      (curBoard, curCoord) <- mPos
      tryMove piece move curCoord curBoard

canEat :: Side -> Board -> Bool
canEat side board = null $ getPossibleEats side board

getPossibleEats :: Side -> Board -> [(Board, Coord)]
getPossibleEats side board =
  concatMap (\(coord, _) -> getPossibleMoves (Just $ Eat NW) board coord) dump
  where
    dump :: [(Coord, Piece)]
    dump = filter (\(_, piece) -> getSide piece == side) (dumpBoard board)

getPossibleMoves :: Maybe Move -> Board -> Coord -> [(Board, Coord)]
getPossibleMoves lastMove board coord =
  case unField $ getFieldUnsafe board coord of
    Nothing      -> []
    (Just piece) -> processPiece piece
  where
    processPiece :: Piece -> [(Board, Coord)]
    processPiece piece = mapMaybe (\move -> tryMove piece move coord board) moves
      where
        dirs :: [Dir]
        dirs = pieceDirs piece
        moves :: [Move]
        moves = concatMap (\dir ->
          case lastMove of
            Nothing       -> [Eat dir, Walk dir]
            Just (Eat _)  -> [Eat dir]
            Just (Walk _) -> []
          ) dirs

finalize :: Board -> Board
finalize board = setFields promoted board
  where
    getRow :: Coord -> Int
    getRow =  snd . unCoord
    needPromotion :: (Coord, Piece) -> Bool
    needPromotion (coord, piece) = getSide piece == White && getRow coord == 7 || getSide piece == Black && getRow coord == 0
    promoted :: [(Coord, Field)]
    promoted = map (\(coord, piece) -> (coord, toField $ promote piece)) $ filter needPromotion (dumpBoard board)

wm :: Field
wm = toField $ Man White
bm :: Field
bm = toField $ Man Black
c :: Int -> Int -> Coord
c a b = Coord (a, b)

polygon1 :: Board
polygon1 = setFields [(c 0 2, wm), (c 1 3, bm), (c 3 5, bm), (c 7 7, wm)] emptyBoard

polygon2 :: Board
polygon2 = setFields [(c 0 2, wm), (c 1 3, bm), (c 3 5, bm), (c 7 7, wm)] emptyBoard

polygon3 :: Board
polygon3 = setFields [(c 0 2, wm), (c 1 3, bm), (c 3 5, bm), (c 7 7, wm)] emptyBoard
