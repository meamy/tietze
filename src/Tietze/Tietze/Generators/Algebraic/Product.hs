-- | Thie module provides types and functions to encode algebraic products with runtime
-- type checking.

module Tietze.Generators.Algebraic.Product
  ( ProductType
  , promoteToProduct
  , equate
  , compose
  , identity
  ) where
 
-----------------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Generators.Algebraic.Internal.Product
  ( ProductType (..)
  , expandIdentityFor
  )
import Tietze.Generators.Categories (MonoidObj (..))
import Tietze.Maybe
  ( branchJust
  , maybeApply
  )

-----------------------------------------------------------------------------------------
-- * Algebraic Product Types

-- | Promotes a runtime list to a product.
promoteToProduct :: [a] -> ProductType a
promoteToProduct = ProductVal

-- | Implements equate on the underlying lists.
equateLImpl :: (MonoidObj a) => [a] -> [a] -> Maybe Bool
equateLImpl []     []     = Just True
equateLImpl xs     []     = Nothing
equateLImpl []     ys     = Nothing
equateLImpl (x:xs) (y:ys) = branchJust (equateLImpl xs ys)
                                       (\res -> maybeApply (equate x y) (res &&))

-- | Implements compose on the underlying lists.
composeLImpl :: (MonoidObj a) => [a] -> [a] -> Maybe [a]
composeLImpl []     []     = Just []
composeLImpl xs     []     = Nothing
composeLImpl []     ys     = Nothing
composeLImpl (x:xs) (y:ys) = branchJust (composeLImpl xs ys)
                                        (\zs -> maybeApply (compose x y) (: zs))

-- | Implements equate in MonoidObj.
equateImpl :: (MonoidObj a) => ProductType a -> ProductType a -> Maybe Bool
equateImpl ProductID       ProductID       = Just True
equateImpl x               ProductID       = equateImpl ProductID x
equateImpl ProductID       (ProductVal ys) = equateLImpl (expandIdentityFor ys) ys
equateImpl (ProductVal xs) (ProductVal ys) = equateLImpl xs ys

-- | Implements compose in MonoidObj.
composeImpl :: (MonoidObj a) => ProductType a -> ProductType a -> Maybe (ProductType a)
composeImpl ProductID       ProductID       = Just ProductID
composeImpl x               ProductID       = Just x
composeImpl ProductID       y               = Just y
composeImpl (ProductVal xs) (ProductVal ys) = maybeApply (composeLImpl xs ys) ProductVal

-----------------------------------------------------------------------------------------
-- * ProductType Defines a Monoid

instance (MonoidObj a) => MonoidObj (ProductType a) where
    equate   = equateImpl
    compose  = composeImpl
    identity = ProductID
