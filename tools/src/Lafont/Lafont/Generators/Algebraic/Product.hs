-- | Thie module provides types and functions to encode algebraic products with runtime
-- type checking.

module Lafont.Generators.Algebraic.Product (
    ProductType,
    promoteToProduct,
    equate,
    compose,
    identity
) where

import           Lafont.Generators.Algebraic.Internal.Product
import           Lafont.Generators.Categories
import           Lafont.Maybe

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
equateLImpl (x:xs) (y:ys) =
    case equateLImpl xs ys of
        Nothing  -> Nothing
        Just res -> case equate x y of
            Nothing   -> Nothing
            Just isEq -> Just (res && isEq)

-- | Implements compose on the underlying lists.
composeLImpl :: (MonoidObj a) => [a] -> [a] -> Maybe [a]
composeLImpl []     []     = Just []
composeLImpl xs     []     = Nothing
composeLImpl []     ys     = Nothing
composeLImpl (x:xs) (y:ys) =
    case composeLImpl xs ys of
        Nothing -> Nothing
        Just zs -> case compose x y of
            Nothing -> Nothing
            Just z  -> Just (z : zs)

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
composeImpl (ProductVal xs) (ProductVal ys) = maybeApply ProductVal (composeLImpl xs ys)

-----------------------------------------------------------------------------------------
-- * ProductType Defines a Monoid

instance (MonoidObj a) => MonoidObj (ProductType a) where
    equate   = equateImpl
    compose  = composeImpl
    identity = ProductID
