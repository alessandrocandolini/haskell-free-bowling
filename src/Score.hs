{-# LANGUAGE DerivingVia #-}

module Score where

import Outcome
import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Sum(Sum))

pins :: (Num a) => a
pins = 10

newtype Score = Score Int
  deriving (Eq)
  deriving (Show, Read, Ord, Num) via Int
  deriving (Semigroup, Monoid) via Sum Int

totalScore :: NonEmpty Outcome -> Score
totalScore = liftA2 (+) scores bonuses

scores :: NonEmpty Outcome -> Score
scores = foldMap score

score :: Outcome -> Score
score Strike = pins
score (Roll (Pin s1) (Pin s2)) = Score (s1 + s2)

bonuses :: NonEmpty Outcome -> Score
bonuses = const 0
