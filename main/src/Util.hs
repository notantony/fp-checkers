{-# LANGUAGE ScopedTypeVariables #-}

module Util
  ( pullMaybeSnd
  , runPipe
  , readNetworkCfg
  , fromIntegralPair
  , Serializable(..)
  , makeSerialization
  , Marge(..)
  , update
  )
  where

import Control.Monad
  ( (>=>)
  )
import System.IO
  ( withFile
  , hGetLine
  , IOMode(ReadMode)
  )
import Data.Foldable
  ( find
  )
import Data.Maybe
  ( fromJust
  )

pullMaybeSnd :: (a, Maybe b) -> Maybe (a, b)
pullMaybeSnd (a, Nothing) = Nothing
pullMaybeSnd (a, Just b) = Just (a, b)

runPipe :: Monad m => [a -> m a] -> (a -> m a)
runPipe = foldr (>=>) return

readNetworkCfg :: String -> IO (String, String)
readNetworkCfg filename =
  withFile filename ReadMode $ \config -> do
    host <- hGetLine config
    port <- hGetLine config
    return (host, port)

fromIntegralPair :: (Integral a, Integral b, Num c, Num d) => (a, b) -> (c, d)
fromIntegralPair (a, b) = (fromIntegral a, fromIntegral b)

class Serializable a where
  serialize :: a -> String
  deserialize :: String -> a

instance (Serializable a) => Serializable [a] where
  serialize = show . (map serialize)
  deserialize = (map deserialize) . read

instance (Serializable a, Serializable b) => Serializable (a, b) where
  serialize (a, b) = show (serialize a, serialize b)
  deserialize = (\(a, b) -> (deserialize a, deserialize b)) . read

makeSerialization :: forall a . Eq a => [(a, String)] -> (a -> String, String -> a)
makeSerialization pairs = (toS, fromS)
  where
    toS :: a -> String 
    toS obj = snd $ fromJust $ find (\(a, _) -> (a == obj)) pairs
    fromS :: String -> a
    fromS str = fst $ fromJust $ find (\(_, s) -> (s == str)) pairs

update :: Int -> a -> [a] -> [a] --TODO: tests
update index elem arr = take index arr ++ [elem] ++ drop (index + 1) arr  
-- prp :: Name -> Q [Dec]
-- prp = do
--   return $ []

data Marge = As | Bs
