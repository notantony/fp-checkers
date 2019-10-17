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
  , checkDir
  , getPiece
  , switchSide
  , getPossibleMoves
  , tryMove
  , promoteBoard
  , allSides
  , getStarts
  , chooseMove
  ) where

-- import Control.Parallel.Strategies (parMap)
import Data.Maybe (catMaybes, isNothing, listToMaybe, mapMaybe)
import Data.Vector (Vector, fromList, toList, (!), (!?), (//))
import Util (Serializable (..), makeSerialization, pullMaybeSnd, pushPair)

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
  toEnum x = error $ "Cannot make Side enum from " ++ show x

allSides :: [Side]
allSides = [White, Black]

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
  fromEnum coord@(Coord (x, y)) =
    if inBoardRange (x, y)
    then x + y * 8
    else error $ "Bad coord: " ++ (show coord)
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
getFieldUnsafe (Board v) coord =
  v ! fromEnum coord

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

setFields :: [(Coord, Field)] -> Board -> Board
setFields target (Board v) =
  Board $ v // (map (\(coord, field) -> (fromEnum coord, field)) target)

startingPosition :: Board
-- startingPosition = whitePieces `setFields` (setFields blackPieces emptyBoard)
--   where
--     whitePieces :: [(Coord, Field)]
--     whitePieces = zip (generateArea (Coord (0, 0)) (Coord (7, 2))) (repeat (toField $ Man White))
--     blackPieces :: [(Coord, Field)]
--     blackPieces = zip (generateArea (Coord (0, 5)) (Coord (7, 7))) (repeat (toField $ Man Black))

allArea :: [Coord]
allArea = generateArea (Coord (0, 0)) (Coord (7, 7))

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
  deserialize [] = error "Cannot deserialize Walk from empty string"

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
coordToDir coord            = error $ "Cannot convert to coord" ++ show coord

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

promoteBoard :: Board -> Board
promoteBoard board = setFields promoted board
  where
    getRow :: Coord -> Int
    getRow =  snd . unCoord
    needPromotion :: (Coord, Piece) -> Bool
    needPromotion (coord, piece) = getSide piece == White && getRow coord == 7 || getSide piece == Black && getRow coord == 0
    promoted :: [(Coord, Field)]
    promoted = map (\(coord, piece) -> (coord, toField $ promote piece)) $ filter needPromotion (dumpBoard board)

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
    dist = x ^ (2 :: Int) + y ^ (2 :: Int)

-- TODO: "Turkish strike"?
tryMove :: Piece -> Move -> Coord -> Board -> Maybe (Board, Coord)
tryMove piece (Walk dir) coord board =
  case getField board dst of
    Nothing -> Nothing
    Just (Field mPiece) -> case mPiece of
      Nothing  -> Just (setFields [(coord, Field Nothing), (dst, toField piece)] board, dst)
      (Just _) -> Nothing
  where
    dst :: Coord
    dst = coord `sumCoord` dirToCoord dir
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
  if checkMany && checkEat
  then foldr foldSingle (Just (board, coord)) moves
  else Nothing
  where
    isEat :: Move -> Bool
    isEat (Eat _) = True
    isEat _       = False
    checkEat :: Bool
    checkEat = case moves of
      []       -> True
      move : _ -> not (canEat (getSide piece) board) || isEat move
    checkMany :: Bool
    checkMany = all isEat moves || length moves <= 1
    foldSingle :: Move -> Maybe (Board, Coord) -> Maybe (Board, Coord)
    foldSingle move mPos = do
      (curBoard, curCoord) <- mPos
      tryMove piece move curCoord curBoard

canEat :: Side -> Board -> Bool
canEat side board = or produced
  where
    dump :: [(Coord, Piece)]
    dump = filter (\(_, piece) -> getSide piece == side) (dumpBoard board)
    produced :: [Bool]
    produced = map (\(coord, _) -> not $ null $ getPossibleMoves (Just $ Eat NW) board coord) dump

getStarts :: Side -> Board -> [Coord]
getStarts side board = filter getMoves coords
  where
    lastMoveM :: Maybe Move
    lastMoveM = if canEat side board then Just $ Eat NW else Nothing
    coords :: [Coord]
    coords = map fst $ filter (\(_, piece) -> getSide piece == side) (dumpBoard board)
    getMoves :: Coord -> Bool
    getMoves coord = not $ null $ getPossibleMoves lastMoveM board coord

getPossibleMoves :: Maybe Move -> Board -> Coord -> [(Board, Coord, Move)]
getPossibleMoves lastMoveM board coord =
  case unField $ getFieldUnsafe board coord of
    Nothing      -> []
    (Just piece) -> result piece
  where
    tryMoves :: Piece -> (Dir -> Move) -> [(Board, Coord, Move)]
    tryMoves piece moveConstr = catMaybes $ do
      dir <- pieceDirs piece
      move <- [moveConstr dir]
      return ((flip pushPair move) <$> (tryMove piece move coord board))
    result :: Piece -> [(Board, Coord, Move)]
    result piece = case lastMoveM of
      Nothing -> case eatMoves of
        [] -> walkMoves
        _  -> eatMoves
      (Just (Eat _))  -> eatMoves
      (Just (Walk _)) -> []
      where
        walkMoves :: [(Board, Coord, Move)]
        walkMoves = tryMoves piece Walk
        eatMoves :: [(Board, Coord, Move)]
        eatMoves = tryMoves piece Eat

chooseMove :: Int -> Side -> Board -> ([Move], Coord)
chooseMove _ side board = winner --runEval run parMap
  where
    starts :: [Coord]
    starts = getStarts side board
    getLongMoves :: Coord -> [(Board, [Move])]
    getLongMoves = longStep [] board
      where
        longStep :: [Move] -> Board -> Coord -> [(Board, [Move])]
        longStep curMoves curBoard curCoord =
          if null curMoves
          then longer
          else (curBoard, curMoves) : longer
          where
            lastMoveM :: Maybe Move
            lastMoveM = listToMaybe curMoves
            resultMoves :: [(Board, Coord, Move)]
            resultMoves = getPossibleMoves lastMoveM curBoard curCoord
            longer :: [(Board, [Move])]
            longer = do
              (succBoard, succCoord, succMove) <- resultMoves
              longStep (succMove : curMoves) succBoard succCoord
    results :: [(Board, ([Move], Coord))]
    results = do
      start <- starts
      ((resultBoard, resultMoves), resultStart) <- [(long, start) | long <- getLongMoves start]
      return (resultBoard, (resultMoves, resultStart))

    winner :: ([Move], Coord) -- TODO: finish
    winner = snd $ head results

    -- positionScore :: Side -> Board -> Float
    -- positionScore _ _ = 0.0


wm :: Field
wm = toField $ Man White
bm :: Field
bm = toField $ Man Black
c :: Int -> Int -> Coord
c a b = Coord (a, b)

polygon1 :: Board
polygon1 = setFields [(c 0 2, wm), (c 1 3, bm), (c 3 5, bm), (c 7 7, wm)] emptyBoard

startingPosition = polygon1
