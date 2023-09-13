{-# LANGUAGE DerivingVia #-}
module Bowling where
import Data.Semigroup
import Data.Monoid (Sum)
import Data.List.NonEmpty
import Control.Applicative (liftA2)

newtype Score = Pin Int deriving (Eq)
   deriving (Show, Read, Ord, Num) via Int
   deriving (Semigroup, Monoid) via Sum Int

data FrameResult = Strike | Spares | KnockedDown Score Score deriving (Eq,Show)

score :: NonEmpty FrameResult -> Score
score = liftA2 (+) individualScores bonuses


individualScores :: NonEmpty FrameResult -> Score
individualScores = foldMap individualScore

individualScore :: FrameResult -> Score
individualScore Strike = 10
individualScore Spares = 10
individualScore (KnockedDown s1 s2) = s1 + s2

bonuses :: NonEmpty FrameResult -> Score
bonuses = const 0
