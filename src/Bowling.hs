{-# LANGUAGE DerivingVia #-}
module Bowling where
import Data.Semigroup
import Data.Monoid (Sum)
import Data.List.NonEmpty
import Control.Applicative (liftA2)

newtype Score = Pin Int deriving (Eq)
   deriving (Show, Read, Ord, Num) via Int
   deriving (Semigroup, Monoid) via Data.Monoid.Sum Int

data FrameResult = Strike | Spares | KnockedDown Score Score deriving (Eq,Show)

totalScore :: NonEmpty FrameResult -> Score
totalScore = liftA2 (+) scores bonuses

scores :: NonEmpty FrameResult -> Score
scores = foldMap score

score :: FrameResult -> Score
score Strike = 10
score Spares = 10
score (KnockedDown s1 s2) = s1 + s2

bonuses :: NonEmpty FrameResult -> Score
bonuses = const 0
