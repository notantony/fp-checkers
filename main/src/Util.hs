module Util
  ( pullMaybeSnd
  , runPipe
  , readNetworkCfg
  , fromIntegralPair
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