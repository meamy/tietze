-- |

module Lafont.Rewrite.Abstraction where

import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- *

-- | Represents the instruction !appply <NAME> <DIR> <POS>. An apply is considered an
-- abstract derivational proof step, as it does not specify the terms to rewrite.
data Apply = Apply String RulePos RuleDir deriving (Eq,Show)

-- | Provides an abstract view of a derivational proof step. Each relation is either a
-- rewrite (i.e., a concrete step) or a derived relation application (i.e., an abstract
-- step).
type AbsRewrite = Either Rewrite Apply

-- | Provides an abstract view of a derivational proof. In an abstract proof, each step
-- is either a rewrite or the application of a derived relation. In other words, the
-- proof consists of abstract rewrites.
data AbsDerivation = AbsDerivation DerivationSummary [AbsRewrite] deriving (Eq,Show)
