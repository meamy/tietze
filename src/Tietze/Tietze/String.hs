-- | General-purpose string manipulation functions not found in Prelude.

module Tietze.String
  ( isSubstrAt
  , isSubstrOf
  , formatList
  , displayList
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import Data.List
  ( intercalate
  , isPrefixOf
  )
import Data.Maybe (isJust)
import Tietze.Maybe (maybeApply)

-----------------------------------------------------------------------------------------
-- Substring Functions.

-- | Consumes two strings (sub and str). If sub is a substring of str, then returns the
-- first index at which sub appears. Otherwise, returns nothing.
isSubstrAt :: String -> String -> Maybe Int
isSubstrAt []  _           = Just 0
isSubstrAt _   []          = Nothing
isSubstrAt sub str@(_:rst) = if sub `isPrefixOf` str
                             then Just 0
                             else maybeApply (sub `isSubstrAt` rst) (+ 1)

-- | Consumes two strings (sub and str). Returns true if and only if sub is a substring
-- of str.
isSubstrOf :: String -> String -> Bool
isSubstrOf sub str = isJust (sub `isSubstrAt` str)

-----------------------------------------------------------------------------------------
-- Formatting Functions.

-- | Consumes a formatting function f list [x1,x2,...,xn]. Returns a string of the form
--     (f x1) ++ "," + (f x2) ++ "," ... + "," (f xn)
-- where each xi is showable.
formatList :: (a -> String) -> [a] -> String
formatList f list = intercalate "," $ map f list

-- | Specializes formatList to use the default show.
displayList :: (Show a) => [a] -> String
displayList = formatList show
