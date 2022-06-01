-- | General-purpoes parsing functions that are used to build specialized parsers.

module DyadicRewrite.Parse.Common where

import Data.Char
import Data.Maybe

-----------------------------------------------------------------------------------------
-- * Character predicates not define in Data.Char.

-- | Consumes a character c. Returns True if and only if c is a space or a tab.
isSpacing :: Char -> Bool
isSpacing ' '  = True
isSpacing '\t' = True
isSpacing _    = False

-- | Consumes a character c. Returns True if and only if c is alphanumeric or _.
isIdChar :: Char -> Bool
isIdChar '_' = True
isIdChar c   = isAlphaNum c

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

-- | Consumes a predicate over characters (pred) and an input string (str). Attempts to
-- parse (pre, post) = (splitAtFirst pred str), if there is anythin to parse. Otherwise,
-- nothing is returned.
parseNonEmpty :: (Char -> Bool) -> String -> Maybe (String, String)
parseNonEmpty checkChar str = if (pre == "") then Nothing else Just (pre, post)
    where (pre, post) = splitAtFirst checkChar str

-- | Consumes an input string (str). Returns the largest natural number prefix of str
-- coverted to an integer.
parseNat :: String -> Maybe (Int, String)
parseNat str
    | digitStr == "" = Nothing
    | otherwise      = Just ((read digitStr :: Int), post)
    where (digitStr, post) = splitAtFirst isDigit str

-- | Consumes an input string (str). Returns the largest integral prefix of str coverted
-- to an integer, if one exists. Otherwise, returns nothing.
parseInt :: String -> Maybe (Int, String)
parseInt ('-' : str) = case (parseNat str) of
                         Just (digit, post) -> Just ((-1) * digit, post)
                         Nothing            -> Nothing
parseInt str         = parseNat str

-- | Consumes an input string (str). Returns (trimmed, post) where (pre, post) =
-- splitAtFirst isSpacing str and trimmed = (pre != "").
trimSpacing :: String -> (Bool, String)
trimSpacing str = (not (pre == ""), post)
    where (pre, post) = splitAtFirst isSpacing str

-- | Consumes an input string (str). Returns the largest identifier prefix of str, if one
-- exists. Otherwise, returns nothing. An identifier must begin with a non-numeric
-- character.
parseId :: String -> Maybe (String, String)
parseId ""  = Nothing
parseId str
    | (isDigit (head str)) = Nothing
    | otherwise            = parseNonEmpty isIdChar str
