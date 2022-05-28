-- |

module DyadicRewrite.Parse.Common where

import Data.Char
import Data.Maybe
import Data.List.Split (splitOneOf)

-----------------------------------------------------------------------------------------
-- * Generic string splitting functions.

-- | Consumes a predicate over characters (pred) and an input string (str). Returns a
-- pair (pre, post) such that str = pre + post and pre is the maximal prefix of str such
-- that ((foldr (\x y -> (pred y) && y) True pre) == True).
splitAtFirst :: (Char -> Bool) -> String -> (String, String)
splitAtFirst _         []  = ("", "")
splitAtFirst checkChar str = if (checkChar (head str))
                             then let (pre, post) = splitAtFirst checkChar (tail str)
                                  in ((head str) : pre, post)
                             else ("", str)

-- | Consumes an input string (str). Returns the largest non-negative integral prefix of
-- str coverted to an integer.
parseNatInt :: String -> Maybe (Int, String)
parseNatInt str
    | digitStr == "" = Nothing
    | otherwise      = Just ((read digitStr :: Int), post)
    where (digitStr, post) = splitAtFirst isDigit str

-- | Consumes an input string (str). Returns the largest integral prefix of str coverted
-- to an integer.
parseInt :: String -> Maybe (Int, String)
parseInt ('-' : str) = case (parseNatInt str) of
                         Just (digit, post) -> Just ((-1) * digit, post)
                         Nothing            -> Nothing
parseInt str         = parseNatInt str
