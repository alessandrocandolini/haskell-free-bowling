{-# LANGUAGE DerivingVia #-}

module Outcome where

data Outcome
  = Strike
  | Spare Int Int
  | KnockedDown Int Int
  deriving (Eq, Show)

