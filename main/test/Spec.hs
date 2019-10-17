module Main
       ( main
       ) where

import SerializableTests (boardTransformSpec, messageTransformSpec)
import Test.Tasty.Hspec (hspec)

main :: IO ()
main = hspec $ do
  boardTransformSpec
  messageTransformSpec

