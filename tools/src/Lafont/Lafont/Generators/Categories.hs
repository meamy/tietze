{-# LANGUAGE FlexibleInstances #-}

-- | Provides data types for the semantics of categories, their generalizations (e.g.
-- monoidal categories) and their specializations (e.g., monoids).

module Lafont.Generators.Categories where

import Quantum.Synthesis.Ring
import Quantum.Synthesis.Matrix

-----------------------------------------------------------------------------------------
-- * Single Abstract Categories (Abstract Algebraic Types).

-- | Additive Integers: the abelian group of integers under addition.
newtype AddInt = AddInt Int deriving (Eq,Show)

-- | Multiplicative Integers: the commutative monoid of integers under multiplication.
newtype MultInt = MultInt Int deriving (Eq,Show)

-- | A monoid in the sense of abstract algebra. Typically, this class describes a set of
-- elements that are equipped with an association composition operator and an identity
-- constant. More generally, this class describes a monoid object in a monoidal category.
-- When M is monoid object in the Category of Sets with Cartesian product, the two
-- definitions coincide.
class (Eq a) => MonoidObj a where
    compose :: a -> a -> a
    identity :: a

instance MonoidObj MultInt where
    compose (MultInt x) (MultInt y) = MultInt (x * y)
    identity                        = MultInt 1

instance MonoidObj AddInt where
    compose (AddInt x) (AddInt y) = AddInt (x + y)
    identity                      = AddInt 0

instance MonoidObj (Matrix Four Four Dyadic) where
    compose a b = a * b
    identity    = 1

instance MonoidObj (Matrix Eight Eight Dyadic) where
    compose a b = a * b
    identity    = 1
