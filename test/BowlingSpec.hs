{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedLists #-}
module BowlingSpec where

import Bowling
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck.Property
import Test.QuickCheck
import Data.List.NonEmpty(NonEmpty)
import qualified Data.List.NonEmpty as N
import GHC.Exts
import Control.Comonad (extend)
import Control.Monad

scoreGen :: Score -> Gen Score
scoreGen (Score max) = Score <$> chooseInt (0, max)

knockedDownGen :: Gen Outcome
knockedDownGen = do
   r1 <- scoreGen (totalPins -2)
   r2 <- scoreGen (r1 - 1)
   return $ KnockedDown r1 r2

spareGen :: Gen Outcome
spareGen = do
   r1 <- scoreGen (totalPins -1)
   r2 <- scoreGen r1
   let r3 = if (r1 + r2 == totalPins) then r2
            else totalPins - r1
   return $ KnockedDown r1 r3

knockedDownsGen :: Gen (NonEmpty Outcome)
knockedDownsGen = (N.:|) <$> knockedDownGen <*> listOf knockedDownGen

frameResultGen ::  Gen Outcome
frameResultGen = oneof [knockedDownGen, pure Strike, spareGen]

newtype Outcomes = Outcomes  { outcomes :: NonEmpty Outcome }  deriving (Eq,Show)

instance IsList Outcomes where
   type Item Outcomes = Score
   fromList = unsafeParseScores
   toList = concatMap toRolls . N.toList . outcomes

unsafeParseScores :: [Score] -> Outcomes
unsafeParseScores s = case (parseScores s) of
      Just l -> l
      Nothing -> error "cannot parse"

parseScores :: [Score] -> Maybe Outcomes
parseScores = N.nonEmpty >=> (fmap Outcomes .  sequenceA . extend parseScore)

parseScore :: NonEmpty Score -> Maybe Outcome
parseScore (n N.:| _ ) | n == totalPins = Just Strike
parseScore (a N.:| ( b : _)) | a + b == totalPins = Just $ Spare a b
                             | otherwise = Just $ KnockedDown a b
parseScore _ = Nothing

spec :: Spec
spec = describe "Bowling" $ do
  describe "parsing from string (used for short notation for other tests)" $ do
     it "parseScore for valid list of partial knock downs" $
        parseScore [2,3,4,2] `shouldBe` (Just (KnockedDown 2 3))
     it "parseScores for valid list of partial knock downs" $
        parseScores [2,3,4,2] `shouldBe` (Just (Outcomes [(KnockedDown 2 3), (KnockedDown 4 2) ]))

     it "with only partial knocked down" $
        [2,3,4,2,1,7] `shouldBe` Outcomes [
              (KnockedDown 2 3),
              (KnockedDown 4 2),
              (KnockedDown 1 7)
             ]


  describe "Score evaluation" $ do
     it "example without bonuses" $
        totalScore [(KnockedDown 5 3), (KnockedDown 2 3), (KnockedDown 0 1 )] `shouldBe` 14

     prop "when outcomes are always partial knock downs, total scores are just the normal score " $
      forAll knockedDownsGen $ \ s -> totalScore s == scores s
