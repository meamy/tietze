-- | Parsing functions for monoidal words.

module DyadicRewrite.Parse.MonWords where

import DyadicRewrite.Common
import DyadicRewrite.Parse.Common

-----------------------------------------------------------------------------------------
-- * Utilities to Parse Symbol Arguments.

-- | Helper function to parse: [ <NAT> ].
parseParam :: String -> Maybe (Int, String)
parseParam []        = Nothing
parseParam ('[':str) =
    case (parseNat str) of
        Just (n, post) -> case (parseFromSeps ["]"] post) of
            Just (_, post') -> Just (n, post')
            Nothing         -> Nothing
        Nothing -> Nothing
parseParam _ = Nothing

-- | Consumes a string (str). If there exists a string pre = [i1][i2][i3]...[in] such
-- that i1, i2, i3, ..., in are natural numbers, str = pre + post, and pre is the maximal
-- such prefix, then ([i1,i2,i3,...,in], post) is returned. Otherwise, nothing is
-- returned.
parseParams :: String -> ([Int], String)
parseParams str =
    case (parseParam str) of
        Just (param, post) -> let (params, post') = (parseParams post)
                              in (param:params, post')
        Nothing -> ([], str)

-----------------------------------------------------------------------------------------
-- * Utilities Parse Sybmol Names.

-- | Consumes a string (str). If there exists a string pre of the form <ID><PARAMS> that
-- str = pre + post, then returns (Symbol <ID> <PARAM>, post) where pre is the maximal
-- such prefix. Otherwise, nothing is returned.
parseSymbol :: String -> Maybe (Symbol, String)
parseSymbol str = 
    case (parseId str) of
        Just (id, post) -> let (params, post') = (parseParams post)
                           in Just ((Symbol id params), post')
        Nothing -> Nothing

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
joinAndParseMonWord symb str =
    case (parseNonEmptyMonWord str) of
        Just (word, post) -> Just (symb:word, post)
        Nothing           -> Nothing

-- | Consumes a string (str). If there exists a monoidal word pre = G1.G2.G3...Gn such
-- that G1, G2, G3, ..., Gn are generator symbols, str = pre + post, and pre is the
-- maximal such prefix of str, then ([G1, G2, G3, ..., Gn], post) is returned.
-- Otherwise, nothing is returned.
--
-- Mutually Depends On: joinAndParseMonWord
parseNonEmptyMonWord :: String -> Maybe (MonWord, String)
parseNonEmptyMonWord str =
    case (parseSymbol str) of
        Just (symb, post) -> case (parseMonWordSep post) of
            Just (MonWordEnd, post') -> Just (symb:[], post')
            Just (MonWordDot, post') -> (joinAndParseMonWord symb post')
            Nothing                  -> Nothing
        Nothing -> Nothing

-- | Consumes a string (str). If str is epsilon, then an empty string is returned.
-- Otherwise, the string is parsed according to parseNonEmptyMonWord.
parseMonWord :: String -> Maybe (MonWord, String)
parseMonWord ('ε':post) = Just ([], post)
parseMonWord str        = parseNonEmptyMonWord str
