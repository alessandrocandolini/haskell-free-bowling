{-# LANGUAGE OverloadedLists #-}
module ScoreSpec where

import Outcome
import Score
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck
import Data.List.NonEmpty

scoreGen :: Int -> Gen Int
scoreGen max = chooseInt (0, max)

knockedDownGen :: Gen Outcome
knockedDownGen = do
   r1 <- scoreGen (pins -2)
   r2 <- scoreGen (r1 - 1)
   return $ KnockedDown r1 r2

knockedDownsGen :: Gen (NonEmpty Outcome)
knockedDownsGen = (:|) <$> knockedDownGen <*> listOf knockedDownGen

frameResultGen ::  Gen Outcome
frameResultGen = oneof [knockedDownGen, pure Strike]


spec :: Spec
spec = describe "Bowling" $ do
  describe "Score evaluation" $ do
     it "example without bonuses" $
        totalScore [(KnockedDown 5 3), (KnockedDown 2 3), (KnockedDown 0 1 )] `shouldBe` 14

     prop "when outcomes are always partial knock downs, total scores are just the normal score " $
      forAll knockedDownsGen $ \ s -> totalScore s == scores s
