module SerializableTests
  ( boardTransformSpec
  , messageTransformSpec
  ) where

import Board
import Server
import Test.Tasty.Hspec (Expectation, Spec, describe, it, shouldBe)
import Util

transform :: Serializable a => a -> a
transform = deserialize . serialize

assertStable :: (Show a, Eq a, Serializable a) => a -> Expectation
assertStable a = a `shouldBe` transform a

boardTransformSpec :: Spec
boardTransformSpec =
  describe "Board serilaization stable" $ do
    it "startingPosition" $ assertStable startingPosition
    it "board transmit" $ assertStable (startingPosition, White)
    it "Move" $ assertStable (Eat SW)
    it "Dir" $ assertStable SW
    it "Coord" $ assertStable $ Coord (3, 2)

messageTransformSpec :: Spec
messageTransformSpec =
  describe "Message serilaization stable" $ do
    it "board" $ assertStable $ makeMsg "board" (startingPosition, White)
    it "str" $ assertStable $ makeStrMsg "Hello, World"
    it "finish" $ assertStable $ makeFinMsg "Black wins!!"
    it "move" $ assertStable $ makeMsg "move" (Coord (0, 0), [Eat NW, Eat NW, Eat NW])
    it "sample1" $ assertStable $ makeMsg "mydata" "test-string"
    it "sample2" $ assertStable $ makeMsg "test" (([White, Black], (startingPosition, [1, 2, 3] :: [Int])), Eat NW)
    it "sample3" $ assertStable $ makeMsg "test" ("qwe", "esdssadw")
