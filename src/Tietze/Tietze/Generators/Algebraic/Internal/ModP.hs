-- | Internals for ModP. Enables unit testing.

module Tietze.Generators.Algebraic.Internal.ModP
  ( ModP (..)
  , ArithModP (..)
  ) where

-----------------------------------------------------------------------------------------
-- * Integral Types Modulo Integers

-- | An abstract notion of reduction modulo an integer. A non-functional requirement of
-- ModP is that (+) :: a -> Int -> a and (>) :: a -> Int -> Bool have definitions
-- consistent with the usual understanding of the integers. Then given x of type a and an
-- integer p, the intent of pmod is to return a value y such that y maximizes the integer
-- n in x = y + pn for y > 0. In practice, pmod need only satisfy idempotence in the
-- first coordinate. That is, (x `pmod` p) `pmod` p = x `pmod` p.
class ModP a where
    pmod :: a -> Int -> a

-----------------------------------------------------------------------------------------
-- * Arithmetic Modulo Integers

-- | Encodes an arithmetic structure modulo some positive integer p. To enable runtime
-- parsing, the integer p is encoded as a field rather than a type parameter.
--
-- The special constructor ArithID represents the identity element modulo any p. If e is
-- the identity element of a, then ArithID equates to (ArithModP e p) for any p. However,
-- equality is a partial function, with (ArithModP e p) incomparable with (ArithModP e q)
-- for all p != q.
data (ModP a) => ArithModP a = ArithID | ArithModP a Int deriving (Show, Eq)
