{-# LANGUAGE DerivingVia #-}
module Bowling where
import Data.Semigroup
import Data.Monoid (Sum)
import Data.List.NonEmpty
import Control.Applicative (liftA2)

pins :: Num a => a
pins = 10

newtype Score = Score Int deriving (Eq)
   deriving (Show, Read, Ord, Num) via Int
   deriving (Semigroup, Monoid) via Data.Monoid.Sum Int

data Outcome = Strike | Spares | KnockedDown Score Score deriving (Eq,Show)

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
