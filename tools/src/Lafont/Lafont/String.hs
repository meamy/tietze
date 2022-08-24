-- | General-purpose string manipulation functions not found in Prelude.

module Lafont.String (
    isSubstrAt,
    isSubstrOf
) where

import           Data.List
import           Data.Maybe
import           Lafont.Maybe

-----------------------------------------------------------------------------------------
-- Substring Functions.

-- | Consumes two strings (sub and str). If sub is a substring of str, then returns the
-- first index at which sub appears. Otherwise, returns nothing.
isSubstrAt :: String -> String -> Maybe Int
isSubstrAt []  _  = Just 0
isSubstrAt _   [] = Nothing
isSubstrAt sub str
    | null sub             = Just 0
    | null str             = Nothing
    | sub `isPrefixOf` str = Just 0
    | otherwise            = maybeApply (+ 1) (sub `isSubstrAt` tail str)

-- | Consumes two strings (sub and str). Returns true if and only if sub is a substring
-- of str.
isSubstrOf :: String -> String -> Bool
isSubstrOf sub str = isJust (sub `isSubstrAt` str)
