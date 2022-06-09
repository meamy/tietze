-- | Realizations of SemParser.

module DyadicRewrite.Parse.Semantics where

import DyadicRewrite.Parse.GeneratorFile

-----------------------------------------------------------------------------------------
-- * Monoidal semantics.

-- | Always returns an error message since free monoids do not have semantics beyond
-- their generators and relations.
parseMonoidalSem :: SemParser ()
parseMonoidalSem _ = Left "Monoidal generators do not support semantics"
