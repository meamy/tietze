-- | General-purpoes parsing functions that are used to build specialized parsers.

module Lafont.Parse.Common where

import Data.Char
import Data.List
import Lafont.Common
import Lafont.Maybe

-----------------------------------------------------------------------------------------
-- * Common Parsing Errors.

-- | General-purpose parsing errors.
data ParserError = ImplError String
                 | DuplicateProp String
                 | UnknownProp String
                 | UnexpectedSymbol Int
                 | UnexpectedEOL
                 | UnexpectedEOF
                 | ExpectedEOF
                 | UnknownParseError
                 deriving (Eq,Show)

instance Display ParserError where
    display (ImplError msg)        = "Implementation error! " ++ msg
    display (DuplicateProp prop)   = "Property set twice (" ++ prop ++ ")."
    display (UnknownProp prop)     = "Unknown property (" ++ prop ++ ")."
    display (UnexpectedSymbol pos) = "Unexpected symbol at " ++ (show pos) ++ "."
    display UnexpectedEOL          = "Unexpected end-of-line."
    display UnexpectedEOF          = "Unexpected end-of-file."
    display ExpectedEOF            = "Expected end-of-file."
    display UnknownParseError      = "Parser failed unexpected."

-- | Consumes a string (full) and the substring upon which parsing failed (unparsed).
-- Returns the position in full at which parsing failed (zero indexed).
getErrPos :: String -> String -> Int
getErrPos full unparsed = (length full) - (length unparsed)

-- | Consumes a string (full), the substring upon which parsing failed (unparsed), and the
-- position at which a parsing error was reached within unparsed (pos). Returns the
-- position relative to full.
relToAbsErrPos :: String -> String -> Int -> Int
relToAbsErrPos full unparsed pos = (getErrPos full unparsed) + pos

-- | Helper function to propogation common parsing errors from a callee parsing function
-- to a caller parsing function. For example, if an error occurs at index 5 of substr,
-- and if substr appears at index 7 of str, then the error is updated to index 12.
--
-- Note: All cases are stated explicitly, so that adding a new positional error without
-- updating this method will result in a compile-time type error.
propCommonErr :: String -> String -> ParserError -> ParserError
propCommonErr str substr err =
    case err of
        (ImplError msg)        -> ImplError msg
        (DuplicateProp prop)   -> DuplicateProp prop
        (UnknownProp prop)     -> UnknownProp prop
        (UnexpectedSymbol pos) -> UnexpectedSymbol (update pos)
        UnexpectedEOL          -> UnexpectedEOL
        UnexpectedEOF          -> UnexpectedEOF
        ExpectedEOF            -> ExpectedEOF
        UnknownParseError      -> UnknownParseError
    where update pos = relToAbsErrPos str substr pos

-----------------------------------------------------------------------------------------
-- * Character Predicate (Not Define in Data.Char).

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
-- * Generic String Splitting Functions.

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
parseInt ('-':str) = maybeApply (\(num, post) -> ((-1) * num, post)) (parseNat str)
parseInt str = parseNat str

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
parseSep sep str = stripPrefix sep trimmed
    where trimmed = snd (trimSpacing str)

-- | Consumes a list of separators (seps) and an input string. If seps if the first such
-- separator in seps such that (Just post) = (parseSep sep str), then (sep, post) is
-- returned. Otherwise, nothing is returned.
parseFromSeps :: [String] -> String -> Maybe (String, String)
parseFromSeps []         str = Nothing
parseFromSeps (sep:seps) str =
    case (parseSep sep str) of -- Can trim once to optimize.
        Just post -> Just (sep, post)
        Nothing   -> parseFromSeps seps str

-- | Consumes a line and strips comments (postfixes starting with '--').
stripComments :: String -> String
stripComments ""          = ""
stripComments ('-':'-':_) = ""
stripComments (c:line)    = c:(stripComments line)

-- | Consumes a line and removes both leading spacing a trailing comments.
cleanLine :: String -> String
cleanLine str = stripComments (snd (trimSpacing str))

-----------------------------------------------------------------------------------------
-- * Conditional Parsing.

-- | Consumes a string (str), and two values (lval and rval). If str contains non-spacing
-- characters, then lval is returned. Otherwise, rval is returned.
branchOnSpacing :: String -> a -> b -> Either a b
branchOnSpacing str lval rval = let trimmed = (snd (trimSpacing str))
                                in if (trimmed == "") then (Right rval) else (Left lval)

-- | Consumes a string (str), and two values of the same type (fval and tval). If str
-- contains non-spacing characters, then fval is returned. Otherwise, rval is returned.
iteOnSpacing :: String -> a -> a -> a
iteOnSpacing str fval tval =
    case (branchOnSpacing str fval tval) of
        Left v  -> v
        Right v -> v

-----------------------------------------------------------------------------------------
-- * Line Parsing.

-- | Consumes a list of lines in a file. If all lines are empty after stripping comments
-- and whitespace, then nothing is retrned. Otherwise, an ExpectedEOF error is returned
-- with the number of the non-empty line.
parseEOFSpacing :: [String] -> Int -> Maybe (Int, ParserError)
parseEOFSpacing []           _   = Nothing
parseEOFSpacing (line:lines) num =
    case (snd (trimSpacing stripped)) of
        ""   -> parseEOFSpacing lines (num + 1)
        text -> Just (num, ExpectedEOF)
    where stripped = stripComments line
