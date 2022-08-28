-- | General-purpose Maybe utilities not found in Prelude.

module Lafont.Maybe (
    branchJust,
    branchNothing,
    maybeApply
) where

-----------------------------------------------------------------------------------------
-- Maybe Value Manipulation.

-- | Consumes a (Maybe a) value and a function f from type a to type (Maybe b). If the
-- Maybe argument wraps a value x of type a, then (f x) is returned. Otherwise, nothing
-- is returned.
branchJust :: Maybe a -> (a -> Maybe b) -> Maybe b
branchJust Nothing  _ = Nothing
branchJust (Just x) f = f x

-- | Consumes two (Maybe a) values. If the first Maybe argument wraps a value x of type
-- a, then (Just x) is returned. Otherwise, the second (Maybe a) argument is returned.
-- The intended use-case for this method is that when the first argument is Nothing, a
-- new value is computed, which may itself be Nothing as well.
branchNothing :: Maybe a -> Maybe a -> Maybe a
branchNothing Nothing  branch = branch
branchNothing (Just x) _      = Just x

-- | Consumes a value of type (Maybe a) and function f from type a to type b. If the
-- Maybe argument wraps a value x of type a, then a (Maybe b) value wrapping (f x) is
-- returned. Otherwise, nothing is returned.
maybeApply :: Maybe a -> (a -> b) -> Maybe b
maybeApply maybe f = branchJust maybe (Just . f)
