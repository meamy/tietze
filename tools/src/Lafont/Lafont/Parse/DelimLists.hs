-- | This module provides functions to parse deliminated lists.

module Lafont.Parse.DelimLists (
    parseList
) where

import Lafont.Parse.Common

-----------------------------------------------------------------------------------------
-- * Utility Types.

-- |
type Tokenizer a = String -> Maybe (a, String)

-----------------------------------------------------------------------------------------
-- * Functions to Parse Deliminated Lists.

-- | See parseList. This method begins parsing from the deliminator symbol. If not
-- deliminator is found, then the string is assumed to be finished.
--
-- Note: Mutually depends on parseList.
parseListDelim :: Tokenizer a -> Char -> String -> Maybe ([a], String)
parseListDelim _        _     []   = Just ([], "")
parseListDelim getToken delim line =
    case parseSep [delim] line of
        Nothing   -> Just ([], line)
        Just rest -> parseList getToken delim rest

-- | Consumes a token parsing function (getToken), a list deliminating character (delim),
-- and a string (line). Attempts to parse line as a sequence of tokens, separated by the
-- delim symbol (optionally with spacing). The string must be of at least length 1.
--
-- Note: Mutually depends on parseListDelim.
parseList :: Tokenizer a -> Char -> String -> Maybe ([a], String)
parseList _        _     []   = Nothing
parseList getToken delim line =
    case getToken trimmed of
        Nothing          -> Nothing
        Just (tok, rest) -> case parseListDelim getToken delim rest of
            Nothing           -> Nothing
            Just (res, after) -> Just (tok : res, after)
    where (_, trimmed) = trimSpacing line
