{-# LANGUAGE FlexibleInstances #-}

-- | Provides data types for the semantics of categories, their generalizations (e.g.
-- monoidal categories) and their specializations (e.g., monoids).

module Lafont.Generators.Categories (
    AddInt ( .. ),
    MultInt ( .. ),
    MonoidObj( .. )
) where

import qualified Quantum.Synthesis.Matrix as QMat
import qualified Quantum.Synthesis.Ring   as QRing

-----------------------------------------------------------------------------------------
-- * Single Object Categories (Abstract Algebraic Types).

-- | Additive Integers: the abelian group of integers under addition.
newtype AddInt = AddInt Int deriving (Eq,Show)

-- | Multiplicative Integers: the commutative monoid of integers under multiplication.
newtype MultInt = MultInt Int deriving (Eq,Show)

-- | A monoid in the sense of abstract algebra. Typically, this class describes a set of
-- elements that are equipped with an association composition operator and an identity
-- constant. More generally, this class describes a monoid object in a monoidal category.
-- When M is monoid object in the Category of Sets with Cartesian product, the two
-- definitions coincide.
--
-- Note that sometimes it is impossible to do all type-checking (for user input) at
-- compile time. This means that not all operations are well-defined. In these cases, no
-- value should be returned. For this reason, compare and compose maybe return values.
class MonoidObj a where
    equate :: a -> a -> Maybe Bool
    compose :: a -> a -> Maybe a
    identity :: a

instance MonoidObj MultInt where
    equate   a           b           = Just (a == b)
    compose  (MultInt x) (MultInt y) = Just (MultInt (x * y))
    identity                         = MultInt 1

instance MonoidObj AddInt where
    equate   a           b         = Just (a == b)
    compose  (AddInt x) (AddInt y) = Just (AddInt (x + y))
    identity                       = AddInt 0

instance MonoidObj (QMat.Matrix QMat.Four QMat.Four QRing.Dyadic) where
    equate   a b = Just (a == b)
    compose  a b = Just (a * b)
    identity     = 1

instance MonoidObj (QMat.Matrix QMat.Eight QMat.Eight QRing.Dyadic) where
    equate   a b = Just (a == b)
    compose  a b = Just (a * b)
    identity     = 1
