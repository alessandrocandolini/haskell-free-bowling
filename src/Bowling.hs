{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE GADTs #-}
module Bowling where

import Outcome

data Bowling a where
  NewFrame :: Bowling a
  FrameOutcome :: Outcome -> Bowling a

