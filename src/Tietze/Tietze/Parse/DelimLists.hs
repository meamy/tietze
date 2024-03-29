{-# LANGUAGE TupleSections #-}

-- | This module provides functions to parse deliminated lists.

module Tietze.Parse.DelimLists
  ( Tokenizer
  , parseList
  , parseBracedList
  , parseTuple
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import           Data.Bifunctor
import           Tietze.Maybe
import           Tietze.Parse.Common

-----------------------------------------------------------------------------------------
-- * Utility Types.

-- | Function to parse the tokens in a list. Each token is of type a. If just (tok, rest)
-- is returned, then tok is the token parsed and rest is the remaining string. If nothing
-- is returned, then parsing fails.
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
-- delim symbol (optionally with spacing). The string must be of at least length 1. If
-- parsing a list of positive length fails, then nothing is returned.
--
-- Note: Mutually depends on parseListDelim.
parseList :: Tokenizer a -> Char -> String -> Maybe ([a], String)
parseList _        _     []   = Nothing
parseList getToken delim line =
    branchJust (getToken trimmed)
        (\(tok, rest) -> maybeApply (parseListDelim getToken delim rest)
                         (Data.Bifunctor.first (tok :)))
    where (_, trimmed) = trimSpacing line

-----------------------------------------------------------------------------------------
-- * Functions to Parse Deliminated Lists in Braces.

-- | Consumes a right bracing character (rbrace), a list of tokens (list), and the end of
-- a string (line). If line is prefixed by rbrace (optionally with spacing), then list
-- together with the rest of line are returned. Otherwise, nothing is returned.
parseRBrace :: Char -> [a] -> String -> Maybe ([a], String)
parseRBrace rbrace list line = maybeApply (parseSep [rbrace] line) (list,)

-- | Consumes a token parsing function (getToken), a list deliminating character (delim),
-- a pair of bracing characters (lbrace and rbrace for [l]eft and [r]ight), and a string
-- (line). attempts to parse line as a sequence of tokens, separated by the delim symbol
-- (optionally with spacing) enclosed by lbrace and rbrace. If the body of the list
-- consists of whitespace, then the empty string is returned. If parsing a list fails,
-- then nothing is returned.
parseBracedList :: Tokenizer a -> Char -> Char -> Char -> String -> Maybe ([a], String)
parseBracedList getToken delim lbrace rbrace line =
    branchJust (parseSep [lbrace] line)
        (\body -> case parseList getToken delim body of
            Nothing          -> parseRBrace rbrace [] body
            Just (list, end) -> parseRBrace rbrace list end)

-- | Specializes parseBracedList to tuples.
parseTuple :: Tokenizer a -> String -> Maybe ([a], String)
parseTuple getToken = parseBracedList getToken delim lbrace rbrace
    where delim = ','
          lbrace = '('
          rbrace = ')'
