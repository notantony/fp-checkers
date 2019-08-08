module Board
  ( Piece(..)
  , Board(..)
  ) where

import Data.Vector
import Control.Monad(join)

data Piece = Checker | Qween
  deriving (Show, Eq)

newtype Board = Board (Vector (Maybe Piece))
  deriving (Show, Eq)

newtype Coord = Coord (Int, Int)
  deriving (Show, Eq)

fieldByCoord :: Board -> Coord -> Maybe Piece
fieldByCoord (Board b) (Coord c) = join $ (b !? index)
  where
    x :: Int
    x = fst c
    y :: Int
    y = snd c
    index :: Int
    index = x + y * 8

someFunc :: IO ()
someFunc = putStrLn "someFunc"