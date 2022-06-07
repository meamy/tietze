-- | General-purpoes parsing functions that are used to build specialized parsers.

module DyadicRewrite.Parse.Common where

import Data.Char
import Data.List

-----------------------------------------------------------------------------------------
-- * Common Parsing Errors.

-- | General-purpose parsing errors.
data ParserError = UnexpectedSymbol Int
                 | UnexpectedEOL
                 | UnexpectedEOF
                 | UnknownParseError
                 deriving (Eq)

instance Show ParserError where
    show (UnexpectedSymbol n) = "Unexpected symbol at " ++ (show n) ++ "."
    show UnexpectedEOL        = "Unexpected end-of-line."
    show UnexpectedEOF        = "Unexpected end-of-file."
    show UnknownParseError    = "Parser failed unexpected."

-- | Consumes a string (full) and the substring upon which parsing failed (unparsed).
-- Returns the position in full at which parsing failed (zero indexed).
getErrPos :: String -> String -> Int
getErrPos full unparsed = (length full) - (length unparsed)

-- | Consumes a string (full), the substring upon which parsing failed (unparsed), and the
-- position at which a parsing error was reached within unparsed (pos). Returns the
-- position relative to full.
relToAbsErrPos :: String -> String -> Int -> Int
relToAbsErrPos full unparsed pos = (getErrPos full unparsed) + pos

-----------------------------------------------------------------------------------------
-- * Character predicates not define in Data.Char.

-- | Consumes a character c. Returns True if and only if c is a space or a tab.
isSpacing :: Char -> Bool
isSpacing ' '  = True
isSpacing '\t' = True
isSpacing _    = False

-- | Consumes a character c. Returns True if and only if c is alphanumeric or _. Note
-- that ε is a reserved character (the empty string).
isIdChar :: Char -> Bool
isIdChar 'ε' = False
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
parseInt ('-':str) = case (parseNat str) of
                         Just (digit, post) -> Just ((-1) * digit, post)
                         Nothing            -> Nothing
parseInt str       = parseNat str

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

-- | Consumes a separator (sep) and an input string (str). Assume there exists at least
-- one prefix of str of the form ( )*sep. Then let pre and post be strings such that pre
-- is the maximal such prefix and str = pre + post. If post exists, then post is
-- returned. Otherwise, nothing is returned. Requires that sep does not contain spacing.
parseSep :: String -> String -> Maybe String
parseSep sep str = case (stripPrefix sep trimmed) of
                       Just post -> Just post
                       Nothing   -> Nothing 
    where trimmed = snd (trimSpacing str)

-- | Consumes a list of separators (seps) and an input string. If seps if the first such
-- separator in seps such that (Just post) = (parseSep sep str), then (sep, post) is
-- returned. Otherwise, nothing is returned.
parseFromSeps :: [String] -> String -> Maybe (String, String)
parseFromSeps []         str = Nothing
parseFromSeps (sep:seps) str = case (parseSep sep str) of -- Can trim once to optimize.
                                   Just post -> Just (sep, post)
                                   Nothing   -> parseFromSeps seps str

-----------------------------------------------------------------------------------------
-- * Conditional parsing.

-- | Consumes a string (str), and two values (lval and rval). If str contains non-spacing
-- characters, then lval is returned. Otherwise, rval is returned.
branchOnSpacing :: String -> a -> b -> Either a b
branchOnSpacing str lval rval = let trimmed = (snd (trimSpacing str))
                                in if (trimmed == "") then (Right rval) else (Left lval)
