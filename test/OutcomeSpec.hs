{-# LANGUAGE OverloadedLists #-}

module OutcomeSpec where

import Outcome
import Score
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Property

spec :: Spec
spec = describe "Bowling" $ do
  describe "parseScores" $ do
    it "with strike" $ parseScores [1, 2, 5, 5, 10, 2, 3]
      `shouldBe` Just
        [ Roll 1 2
        , Roll 5 5
        , Strike
        , Roll 2 3
        ]
    it "without strike" $ parseScores [1, 2, 5, 5, 2, 3]
      `shouldBe` Just
        [ Roll 1 2
        , Roll 5 5
        , Roll 2 3
        ]
  describe "parseScore" $ do
    it "with strike"
      $ parseScore [1, 2, 5, 5, 10, 2, 3]
      `shouldBe` Just (Roll 1 2)
    it "without strike"
      $ parseScore [1, 2, 5, 5, 2, 3]
      `shouldBe` Just (Roll 1 2)

-- prop "when outcomes are always partial knock downs, total scores are just the normal score " $
-- forAll knockedDownsGen $ \ s -> totalScore s == scores s
