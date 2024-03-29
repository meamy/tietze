-- | Parsing functions for monoidal words.

module Tietze.Parse.MonWords
  ( parseParam
  , parseParams
  , parseSymbol
  , findUnknownGenInMonWord
  , parseNonEmptyMonWord
  , parseMonWord
  , parseLineAsMonWord
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import           Data.Bifunctor
import           Tietze.Common
import           Tietze.Maybe
import           Tietze.Parse.Common

-----------------------------------------------------------------------------------------
-- * Utilities to Parse Symbol Arguments.

-- | Helper function to parse: [ <NAT> ].
parseParam :: String -> Maybe (Int, String)
parseParam []        = Nothing
parseParam ('[':str) =
    case parseNat str of
        Just (n, post) -> maybeApply (parseFromSeps ["]"] post) (\(_, post) -> (n, post))
        Nothing        -> Nothing
parseParam _ = Nothing

-- | Consumes a string (str). If there exists a string pre = [i1][i2][i3]...[in] such
-- that i1, i2, i3, ..., in are natural numbers, str = pre + post, and pre is the maximal
-- such prefix, then ([i1,i2,i3,...,in], post) is returned. Otherwise, nothing is
-- returned.
parseParams :: String -> ([Int], String)
parseParams str =
    case parseParam str of
        Just (param, post) -> let (params, post') = parseParams post
                              in (param : params, post')
        Nothing -> ([], str)

-----------------------------------------------------------------------------------------
-- * Utilities to Parse Symbol Names.

-- | Consumes a string (str). If there exists a string pre of the form <ID><PARAMS> that
-- str = pre + post, then returns (Symbol <ID> <PARAM>, post) where pre is the maximal
-- such prefix. Otherwise, nothing is returned.
parseSymbol :: String -> Maybe (Symbol, String)
parseSymbol str = maybeApply (parseId str) parseImpl
    where parseImpl (id, post) = let (params, post') = parseParams post
                                 in (Symbol id params, post')

-- | Consumes a list of generator names (gens) and a monoidal word (word). Returns the
-- first symbol in monoidal word with either a non-zero number of parameters or a name
-- not in gens. If no such symbol exists, then nothing is returned.
findUnknownGenInMonWord :: [String] -> MonWord -> Maybe Symbol
findUnknownGenInMonWord gens []          = Nothing
findUnknownGenInMonWord gens (symb:word)
    | symbolIsValid = findUnknownGenInMonWord gens word
    | otherwise     = Just symb
    where symbolIsValid = null (args symb) && name symb `elem` gens

-----------------------------------------------------------------------------------------
-- * Monoidal Word Parsing Functions.

-- | Data type used to distinguish separators in monoidal words.
data MonWordSep = MonWordDot | MonWordEnd

-- | Helper method to classify the next separator in a monoidal word. If the separator is
-- a part of the monoidal word, then the characters are consumed. If there is no match,
-- then nothing is returned.
parseMonWordSep :: String -> Maybe (MonWordSep, String)
parseMonWordSep ""         = Just (MonWordEnd, "")
parseMonWordSep (' ':post) = Just (MonWordEnd, ' ':post)
parseMonWordSep ('.':post) = Just (MonWordDot, post)
parseMonWordSep _          = Nothing

-- | Helper method to append a known symbool to the front of a word parsed by a monoidal
-- word parser. If the monoidal word does not parse, then nothing is returned.
--
-- Mutually Depends On: parseNonEmptyMonWord
joinAndParseMonWord :: Symbol -> String -> Maybe (MonWord, String)
joinAndParseMonWord symb str = maybeApply (parseNonEmptyMonWord str)
                                          (Data.Bifunctor.first (symb :))

-- | Helper method to split parseNonEmptyMonWord between MonWordEnd and MonWordDot. If
-- the separator is MonWordEnd, the a return list is started. Otherwise, the parser
-- recurses by calling joinAndParseMonWord.
splitOnSep :: Symbol -> (MonWordSep, String) -> Maybe (MonWord, String)
splitOnSep symb (MonWordEnd, post) = Just ([symb], post)
splitOnSep symb (MonWordDot, post) = joinAndParseMonWord symb post

-- | Consumes a string (str). If there exists a monoidal word pre = G1.G2.G3...Gn such
-- that G1, G2, G3, ..., Gn are generator symbols, str = pre + post, and pre is the
-- maximal such prefix of str, then ([G1, G2, G3, ..., Gn], post) is returned.
-- Otherwise, nothing is returned.
--
-- Mutually Depends On: joinAndParseMonWord
parseNonEmptyMonWord :: String -> Maybe (MonWord, String)
parseNonEmptyMonWord str =
    branchJust (parseSymbol str)
               (\(symb, post) -> branchJust (parseMonWordSep post) (splitOnSep symb))

-- | Consumes a string (str). If str is epsilon, then an empty string is returned.
-- Otherwise, the string is parsed according to parseNonEmptyMonWord.
parseMonWord :: String -> Maybe (MonWord, String)
parseMonWord ('ε':post) = Just ([], post)
parseMonWord str        = parseNonEmptyMonWord str

-----------------------------------------------------------------------------------------
-- * Line Parsing Functions.

-- | Consumes a string (str). If str contains a monoidal word (with optional whitespace
-- and comments), then the monoidal word is returned. Otherwise, nothing is returned.
parseLineAsMonWord :: String -> Maybe MonWord
parseLineAsMonWord line = branchJust (parseMonWord cleaned)
                                     (\(w, post) -> iteOnSpacing post Nothing (Just w))
    where cleaned = cleanLine line
