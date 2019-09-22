{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( boardTex
  , blackManTex
  , whiteManTex
  , blackKingTex
  , whiteKingTex
  , dotTex
  )
where

import Data.FileEmbed
import Data.ByteString(ByteString)
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss

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