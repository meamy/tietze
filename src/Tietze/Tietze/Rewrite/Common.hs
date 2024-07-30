-- | The module provides basic types and functions for working with derivational proofs.

module Tietze.Rewrite.Common
  ( RuleDir (..)
  , RulePos
  , reverseDir
  ) where

-----------------------------------------------------------------------------------------
-- * Basic Types.

-- | The direction of a rule application in a derivational proof. Either left-to-right
-- (L2R) or right-to-left (R2L).
data RuleDir = L2R | R2L deriving (Show, Eq)

-- | The location of a rule application in a derivational proof. Should be non-negative.
-- This requirement is not checked at compile-time.
type RulePos = Int

-----------------------------------------------------------------------------------------
-- * Basic Operations.

-- | Reverses the direction of a RuleDir.
reverseDir :: RuleDir -> RuleDir
reverseDir L2R = R2L
reverseDir R2L = L2R
