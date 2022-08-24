-- | This module provides types and functions for string rewrite rules. In particular,
-- the RewriteRule type describes a rule in a rewrite system and the Rewrite type
-- describes the application of a rewrite rule to a specific string.

module Lafont.Rewrite.Rules (
    RewriteRule ( .. ),
    Rewrite ( .. ),
    isDerivedRule,
    checkRewriteRule,
    applyRewriteRule,
    checkRewrite,
    applyRewrite
) where

import           Data.Maybe
import           Lafont.Common
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Internal.Rules

-----------------------------------------------------------------------------------------
-- * RewriteRules.

-- | A rewrite rule in a rewrite system is a tuple of the form (lhs, rhs) and is usually
-- denoted lhs → rhs (we refer to u → v as a production rule as it produces the word xvy
-- from the word xuy for any words x and y). A rule is said to be **equational** if it is
-- symmetric (that is, both lhs → rhs and rhs → lhs are valid). A rule u →* u' may also
-- be **derivedFrom** a proof of the form u → v1 → v2 → ... → vk → u', where each
-- intermediate rule is valid. In this case, the derivedFrom field is assigned the name
-- of the proof.
data RewriteRule = RewriteRule { lhs         :: MonWord
                               , rhs         :: MonWord
                               , equational  :: Bool
                               , derivedFrom :: Maybe String
                               } deriving (Show,Eq)

-- | Consumes a rule. Returns true if the rule is derived.
isDerivedRule :: RewriteRule -> Bool
isDerivedRule rule = isJust (derivedFrom rule)

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns true if rule matches a prefix of the
-- monoidal word.
checkRewriteRule :: MonWord -> RewriteRule -> RuleDir -> Bool
checkRewriteRule str rule L2R = doesRewriteTermMatch str (lhs rule)
checkRewriteRule str rule R2L = doesRewriteTermMatch str (rhs rule)

-- | Consumes a monoidal word, a rewrite rule, and a boolean flag indicating if the rule
-- is to be applied from left-to-right. Returns the string obtained by applying the
-- rewrite rule. Assumes that checkRewriteRule is true.
applyRewriteRule :: MonWord -> RewriteRule -> RuleDir -> MonWord
applyRewriteRule str rule L2R = applyProductionRule str (lhs rule) (rhs rule)
applyRewriteRule str rule R2L = applyProductionRule str (rhs rule) (lhs rule)

-----------------------------------------------------------------------------------------
-- * Rewrites.

-- | Applies a rewrite rule at the specified position, in the specified direction.
data Rewrite = Rewrite RewriteRule RulePos RuleDir deriving (Show,Eq)

-- | Consumes a monoidal word and a rewrite. Returns true if rule matches at the position
-- indicated by the rewrite.
checkRewrite :: MonWord -> Rewrite -> Bool
checkRewrite str (Rewrite rule pos dir) = impl str pos
    where impl substr n = if n == 0
                          then checkRewriteRule substr rule dir
                          else not (null substr) && impl (tail substr) (n - 1)

-- | Consumes a monoidal word and a rewrite. Returns the string obtained by applying the
-- rewrite. Assumes that checkRewrite is true.
applyRewrite :: MonWord -> Rewrite -> MonWord
applyRewrite str (Rewrite rule pos dir) = impl str pos
    where impl substr n = if n == 0
                          then applyRewriteRule substr rule dir
                          else head substr : impl (tail substr) (n - 1)
