module Util
  ( pullMaybeSnd
  , runPipe
  )
  where

import Control.Monad
  ( (>=>)
  )

pullMaybeSnd :: (a, Maybe b) -> Maybe (a, b)
pullMaybeSnd (a, Nothing) = Nothing
pullMaybeSnd (a, Just b) = Just (a, b)

runPipe :: (Monad m) => [a -> m a] -> (a -> m a)
runPipe = foldr (>=>) return

  