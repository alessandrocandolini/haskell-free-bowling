{-# LANGUAGE DerivingVia #-}

module Outcome where

import Data.Semigroup (Sum (Sum))
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Control.Comonad

newtype Pin = Pin Int
  deriving (Eq)
  deriving (Show, Read, Ord, Num) via Int
  deriving (Semigroup, Monoid) via Sum Int

totalPins :: Pin
totalPins = 10

data Outcome
  = Strike
  | Roll Pin Pin
  deriving (Eq, Show)

strike :: Outcome
strike = Strike

roll :: Pin -> Pin -> Maybe Outcome
roll a b
  | a >= 0 && b >= 0 && a + b <= totalPins = Just (Roll a b)
  | otherwise = Nothing

isSpare :: Outcome -> Bool
isSpare (Roll a b) | a + b == totalPins = True
isSpare _ = False

parseScores :: NonEmpty Pin -> Maybe (NonEmpty Outcome)
parseScores = sequenceA . extend parseScore


data Token = X | Miss | Pins Pin deriving (Eq,Show)

parseScore :: NonEmpty Pin -> Maybe Outcome
parseScore (a N.:| []) | a == totalPins = Just Strike
                     | otherwise = Nothing
parseScore (a N.:| (b : _)) = Just (Roll a b)


