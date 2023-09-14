{-# LANGUAGE DerivingVia #-}
module Bowling where
import Data.Semigroup
import Data.Monoid (Sum)
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as N
import Control.Applicative (liftA2)
import Data.Foldable (fold)
import Control.Comonad (extend)

totalPins :: Num a => a
totalPins = 10

newtype Score = Score Int deriving (Eq)
   deriving (Show, Read, Ord, Num) via Int
   deriving (Semigroup, Monoid) via Data.Monoid.Sum Int

data Outcome = Strike | Spare Score Score | KnockedDown Score Score deriving (Eq,Show)

totalScore :: NonEmpty Outcome -> Score
totalScore = liftA2 (+) scores bonuses

scores :: NonEmpty Outcome -> Score
scores = foldMap score

score :: Outcome -> Score
score Strike = totalPins
score (Spare _ _ ) = totalPins
score (KnockedDown s1 s2) = s1 + s2

bonuses :: NonEmpty Outcome -> Score
bonuses = fold . extend bonus

bonus :: NonEmpty Outcome -> Score
bonus = liftA2 bonus' N.head N.tail

bonus' :: Outcome -> [Outcome] -> Score
bonus' (KnockedDown _ _ ) = const 0
bonus' (Spare _ _ ) = pinNextRoll 1
bonus' Strike = pinNextRoll 2

toRolls :: Outcome -> [Score]
toRolls Strike = pure totalPins
toRolls (Spare s1 s2 ) = [s1, s2]
toRolls (KnockedDown s1 s2) = [s1, s2]


pinNextRoll :: Int -> [Outcome] -> Score
pinNextRoll n = fold . take n . concatMap toRolls

