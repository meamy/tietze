-- | This module provides functions and types to represent (monoids/groups/rings/etc) of
-- integral types modulo integers.

module Tietze.Generators.Algebraic.ModP
  ( ArithModP
  , ModP (..)
  , reduce
  , inclusionModP
  , equate
  , compose
  , identity
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Generators.Algebraic.Internal.ModP
  ( ArithModP (..)
  , ModP (..)
  )
import Tietze.Generators.Categories
  ( AddInt (..)
  , MonoidObj (..)
  , MultInt (..)
  )
import Tietze.Maybe (maybeApply)

-----------------------------------------------------------------------------------------
-- * Integral Types Modulo Integers

instance ModP AddInt where
    pmod (AddInt x) p = AddInt (x `mod` p)

instance ModP MultInt where
    pmod (MultInt x) p = MultInt (x `mod` p)

-----------------------------------------------------------------------------------------
-- * Arithmetic Modulo Integers

-- | Reduces the arithmetic value in an ArithModP value modulo the given prime. When p is
-- zero, no rounding occurs.
reduce :: (ModP a) => ArithModP a -> ArithModP a
reduce ArithID = ArithID
reduce (ArithModP v p)
    | p == 0    = ArithModP v p
    | otherwise = ArithModP (v `pmod` p) p

-- | Given p, (\v -> inclusionModP v p) is the inclusion map from the type a to the type
-- (a modulo p).
inclusionModP :: (ModP a) => a -> Int -> Maybe (ArithModP a)
inclusionModP v p
    | p >= 0    = Just $ reduce (ArithModP v p)
    | otherwise = Nothing

-- | Consumes two integers p1 and p2. Returns true if ArithModP v1 p1 would be composable
-- with ArithModP v2 p2.
incomposablePair :: Int -> Int -> Bool
incomposablePair p1 p2 = invalid || incomposable
    where invalid = p1 < 0 || p2 < 0
          incomposable = p1 /= p2

-- | Implements equate in MonoidObj.
equateImpl :: (ModP a, MonoidObj a) => ArithModP a -> ArithModP a -> Maybe Bool
equateImpl ArithID ArithID         = Just True
equateImpl x       ArithID         = equateImpl ArithID x
equateImpl ArithID (ArithModP v p)
    | p < 0     = Nothing
    | otherwise = equate v identity
equateImpl (ArithModP v1 p1) (ArithModP v2 p2)
    | incomposablePair p1 p2 = Nothing
    | otherwise              = equate v1 v2

-- | Implements compose in MonoidObj.
composeImpl :: (ModP a, MonoidObj a) => ArithModP a -> ArithModP a -> Maybe (ArithModP a)
composeImpl ArithID ArithID = Just ArithID
composeImpl x       ArithID = composeImpl ArithID x
composeImpl ArithID y
    | p < 0         = Nothing
    | otherwise     = Just y
    where (ArithModP _ p) = y
composeImpl (ArithModP v1 p1) (ArithModP v2 p2)
    | incomposablePair p1 p2 = Nothing
    | otherwise              = maybeApply (compose v1 v2) (\v -> reduce (ArithModP v p1))

-----------------------------------------------------------------------------------------
-- * ArithModP Defines a Monoid

instance (ModP a, MonoidObj a) => MonoidObj (ArithModP a) where
    equate   = equateImpl
    compose  = composeImpl
    identity = ArithID
