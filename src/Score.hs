{-# LANGUAGE DerivingVia #-}

module Score where

import Control.Applicative (liftA2)
import Data.List.NonEmpty (NonEmpty)
import Data.Semigroup (Sum(Sum))

pins :: (Num a) => a
pins = 10

newtype Score = Score Int
  deriving (Eq)
  deriving (Show, Read, Ord, Num) via Int
  deriving (Semigroup, Monoid) via Sum Int

data Outcome
  = Strike
  | Spares
  | KnockedDown Score Score
  deriving (Eq, Show)

totalScore :: NonEmpty Outcome -> Score
totalScore = liftA2 (+) scores bonuses

scores :: NonEmpty Outcome -> Score
scores = foldMap score

score :: Outcome -> Score
score Strike = pins
score Spares = pins
score (KnockedDown s1 s2) = s1 + s2

bonuses :: NonEmpty Outcome -> Score
bonuses = const 0
