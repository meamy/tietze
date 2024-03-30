-- | Internals for Product. Enables unit testing.

module Tietze.Generators.Algebraic.Internal.Product
  ( ProductType (..)
  , expandIdentityFor
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Generators.Categories (MonoidObj (..))

-----------------------------------------------------------------------------------------
-- * Algebraic Product Types

-- | Encodes the Cartesian product of algebraic structures. To enable runtime parsing,
-- the cardinality of the product is encoded by a runtime length variable rather than a
-- type parameter.
--
-- The special constructor ProductID represents the identity for products of any length.
-- If e is the identity element of a and f = replicate, then ProductID equates to
-- (ProductVal (f n e)) for any n. However, equality is a partial function, with
-- (ProductVal (f n e)) incomparable with (ProductVal (f m e)) for all n != m.
data ProductType a = ProductID | ProductVal [a] deriving (Eq,Show)

-- | Consumes a list of monoidal objects. Returns a new list, of the same length, whose
-- only element is the identity of a.
expandIdentityFor :: (MonoidObj a) => [a] -> [a]
expandIdentityFor list = replicate (length list) identity
