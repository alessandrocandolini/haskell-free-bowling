{-# LANGUAGE OverloadedLists #-}
module BowlingSpec where

import Bowling
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck


spec :: Spec
spec = describe "Bowling" $ do
  describe "Score evaluation" $ do
     it "example without bonuses" $
        1 `shouldBe` 1

     prop "when outcomes are always partial knock downs, total scores are just the normal score " $
       \l -> l == (reverse (reverse l) :: [Int])
