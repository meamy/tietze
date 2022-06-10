-- | Realizations of SemParser.

module DyadicRewrite.Parse.Semantics where

-----------------------------------------------------------------------------------------
-- * General Semantic Parsing.

-- | A function used to parse a value given a semantic model. Takes as input a textual
-- representation of the semantic value. Returns either a parsing error (as a string) or
-- a semantic model value.
type SemParser a = (String -> Either String a)

-----------------------------------------------------------------------------------------
-- * Monoidal Semantics.

-- | Always returns an error message since free monoids do not have semantics beyond
-- their generators and relations.
parseMonoidalSem :: SemParser ()
parseMonoidalSem _ = Left "Monoidal generators do not support semantics"
