-- | Internals for Rules. Enables unit testing.

module Tietze.Rewrite.Internal.Rules
  ( doesRewriteTermMatch
  , applyProductionRule
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Common (MonWord)

-----------------------------------------------------------------------------------------
-- * RewriteRules.

-- | Consumes a monoidal word and the lhs of a production rule: lhs → rhs. True is
-- returned if the rule is applicable at index .
doesRewriteTermMatch :: MonWord -> MonWord -> Bool
doesRewriteTermMatch _       []           = True
doesRewriteTermMatch []      _            = False
doesRewriteTermMatch (sym:str) (lsym:lhs) = match && doesRewriteTermMatch str lhs
    where match = sym == lsym

-- | Consumes a monoidal word, together with the lhs and rhs of a production rule:
-- lhs → rhs. Returns the monoidal word obtained by applying the production rule at
-- index. Assumes that doesRewriteTermMatch is true.
applyProductionRule :: MonWord -> MonWord -> MonWord -> MonWord
applyProductionRule str     []      []         = str
applyProductionRule str     []      (rsym:rhs) = rsym : applyProductionRule str [] rhs
applyProductionRule (_:str) (_:lhs) rhs        = applyProductionRule str lhs rhs
