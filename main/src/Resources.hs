{-# LANGUAGE TemplateHaskell #-}

module Resources
  ( boardTex
  , blackCheckerTex
  , whiteCheckerTex
  )
where

import Prelude(Bool(True))
import Data.FileEmbed
import Data.ByteString
import Graphics.Gloss.Data.Bitmap
import Graphics.Gloss

boardTex :: Picture
boardTex = bitmapOfByteString 
  860 
  860
  BitmapFormat{ rowOrder = TopToBottom, pixelFormat = PxRGBA }
  $(embedFile "resources/board_windows.rgba")
  True

blackCheckerTex :: Picture
blackCheckerTex = bitmapOfByteString 
  100
  100
  BitmapFormat{ rowOrder = TopToBottom, pixelFormat = PxRGBA }
  $(embedFile "resources/b_checker.rgba")
  True
  
whiteCheckerTex :: Picture
whiteCheckerTex = bitmapOfByteString 
  100
  100
  BitmapFormat{ rowOrder = TopToBottom, pixelFormat = PxRGBA }
  $(embedFile "resources/w_checker.rgba")
  True