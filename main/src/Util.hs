{-# LANGUAGE TemplateHaskell #-} -- TODO

module Util
  ( pullMaybeSnd
  , runPipe
  , readNetworkCfg
  , fromIntegralPair
  , mkConv
  )
  where

    
import Language.Haskell.TH
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

mkConv :: String -> (Int, String) -> Q [Dec]
mkConv name qw = do
  tt <- [| qw |]
  return $ FunD (mkName name) [Clause [] (NormalB tt) []] : []