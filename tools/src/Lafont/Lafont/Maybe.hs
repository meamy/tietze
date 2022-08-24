-- | General-purpose Maybe utilities not found in Prelude.

module Lafont.Maybe (maybeApply) where

-----------------------------------------------------------------------------------------
-- Maybe Value Manipulation.

-- | Consumes a function from type a to type b, and a Maybe value of type a. If the Maybe
-- argument wraps a value x of type a, then a Maybe of type b wrapping (f x) is returned.
-- Otherwise, nothing is returned.
maybeApply :: (a -> b) -> Maybe a -> Maybe b
maybeApply _ Nothing  = Nothing
maybeApply f (Just x) = Just (f x)
