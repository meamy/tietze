-- | Parameterized datatype to associate names and indices with values of a given type.

module Lafont.Named (
    Named(..),
    addToNamedList
) where

-----------------------------------------------------------------------------------------
-- * Named List Management.

-- | Allows a source to be identified with a data value. If multiple values originate
-- from the same source, then identifiers may be used to further distinguish values.
data Named a = Named { source     :: String
                     , identifier :: Int
                     , value      :: a
                     } deriving (Show, Eq)

-- | Takes as input a name (src), a list of named data, a list of unnamed data, and an
-- index (idx). Prepends the unnamed data to the named list using name as a source, and
-- ascending numbers starting from idx as the identifies.
addToNamedList :: String -> [Named a] -> [a] -> Int -> [Named a]
addToNamedList _   nlist []          _   = nlist
addToNamedList src nlist (x:unnamed) idx = Named src idx x : rest
    where rest = addToNamedList src nlist unnamed $ idx + 1
