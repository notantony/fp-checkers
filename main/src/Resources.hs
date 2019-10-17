{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( boardTex
  , blackManTex
  , whiteManTex
  , blackKingTex
  , whiteKingTex
  , dotTex
  , ghostTex
  )
where

import Data.ByteString (ByteString)
import Data.FileEmbed (embedFile)
import Graphics.Gloss (Picture (..))
import Graphics.Gloss.Data.Bitmap (BitmapFormat (..), PixelFormat (..), RowOrder (..),
                                   bitmapOfByteString)

importTex :: Int -> Int -> ByteString -> Picture
importTex x y texData = Translate (fromIntegral x / 2) (fromIntegral y / 2) bmpTex
  where
    bmpTex :: Picture
    bmpTex =
      bitmapOfByteString x y
      BitmapFormat{ rowOrder = TopToBottom, pixelFormat = PxRGBA }
      texData
      True

boardTex :: Picture
boardTex = importTex 860 860 $(embedFile "resources/board_windows.rgba")

blackManTex :: Picture
blackManTex = importTex 100 100 $(embedFile "resources/b_man.rgba")

whiteManTex :: Picture
whiteManTex = importTex 100 100 $(embedFile "resources/w_man.rgba")

blackKingTex :: Picture
blackKingTex = importTex 100 100 $(embedFile "resources/b_king.rgba")

whiteKingTex :: Picture
whiteKingTex = importTex 100 100 $(embedFile "resources/w_king.rgba")

dotTex :: Picture
dotTex = importTex 100 100 $(embedFile "resources/dot.rgba")

ghostTex :: Picture
ghostTex = importTex 100 100 $(embedFile "resources/ghost.rgba")
