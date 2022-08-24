-- | Internals for Rules. Enables unit testing.

module Lafont.Rewrite.Internal.Rules (
    doesRewriteTermMatch,
    applyProductionRule
) where

import           Lafont.Common

-----------------------------------------------------------------------------------------
-- * RewriteRules.

-- | Consumes a monoidal word and the lhs of a production rule: lhs → rhs. True is
-- returned if the rule is applicable at index .
doesRewriteTermMatch :: MonWord -> MonWord -> Bool
doesRewriteTermMatch _   []  = True
doesRewriteTermMatch []  _   = False
doesRewriteTermMatch str lhs = head str == head lhs && match
    where match = doesRewriteTermMatch (tail str) (tail lhs)

-- | Consumes a monoidal word, together with the lhs and rhs of a production rule:
-- lhs → rhs. Returns the monoidal word obtained by applying the production rule at
-- index. Assumes that doesRewriteTermMatch is true.
applyProductionRule :: MonWord -> MonWord -> MonWord -> MonWord
applyProductionRule str []  []  = str
applyProductionRule str []  rhs = head rhs : applyProductionRule str [] (tail rhs)
applyProductionRule str lhs rhs = applyProductionRule (tail str) (tail lhs) rhs
