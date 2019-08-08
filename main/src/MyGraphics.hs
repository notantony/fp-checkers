module MyGraphics
  ( makeTextLarge
  , makeTextNormal
  , toBold
  , loadTextures
  ) where

import Graphics.Gloss

loadTextures :: IO ()
someFunc = putStrLn "someFunc"

makeText :: Float -> Point -> String -> Picture
makeText size (x, y) s = Color white $ Translate x y (Scale size size (Text s))

makeTextNormal :: Point -> String -> Picture
makeTextNormal = makeText 0.25

makeTextLarge :: Point -> String -> Picture
makeTextLarge = makeText 0.5

toBold :: Picture -> Picture
toBold pic = Pictures [pic, Translate 1 0 pic]