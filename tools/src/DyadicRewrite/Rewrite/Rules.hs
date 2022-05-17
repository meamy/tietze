-- | This module provides types and functions for string rewrite rules. In particular
-- RewriteRules rewrite the prefix of a string whereas RewriteOps apply RewriteRules to a
-- specified substring of a string.

module DyadicRewrite.Rewrite.Rules where

import DyadicRewrite.Common (Circuit)

-----------------------------------------------------------------------------------------
-- * RewriteRule

-- | Describes a rewrite rule of the form: lhs <=> rhs.
data RewriteRule = RewriteRule { lhs :: Circuit
                               , rhs :: Circuit
                               } deriving (Show,Eq)

-- | Internal implementation of checkRewriteRule. The first circuit is the string to
-- rewrite. The second circuit is the lhs of the rewrite rule. True is returned if the
-- rule is applicable.
checkRewriteRuleRec :: Circuit -> Circuit -> Bool
checkRewriteRuleRec _   []  = True
checkRewriteRuleRec []  _   = False
checkRewriteRuleRec str lhs = if (head str) == (head lhs)
                              then checkRewriteRuleRec (tail str) (tail lhs)
                              else False

-- | Consumes a circuit, a rewrite rule, and a boolean flag indicating if the rule is to
-- be applied from left-to-right. Returns true if rule matches a prefix of the circuit.
checkRewriteRule :: Circuit -> RewriteRule -> Bool -> Bool
checkRewriteRule str rule True  = checkRewriteRuleRec str (lhs rule)
checkRewriteRule str rule False = checkRewriteRuleRec str (rhs rule)

-- | Internal implementation of applyRewriteRule. The first circuit is the string to
-- rewrite. The second circuit is the lhs of the rewrite rule. The second circuit is the
-- rhs of the rewrite rule. Returns the new circuit.
applyRewriteRuleRec :: Circuit -> Circuit -> Circuit -> Circuit
applyRewriteRuleRec str []  []  = str
applyRewriteRuleRec str []  rhs = (head rhs) : (applyRewriteRuleRec str [] (tail rhs))
applyRewriteRuleRec str lhs rhs = applyRewriteRuleRec (tail str) (tail lhs) rhs

-- | Consumes a circuit, a rewrite rule, and a boolean flag indicating if the rule is to
-- be applied from left-to-right. Returns the string obtained by applying the rewrite
-- rule. Assumes that checkRewriteRule is true. 
applyRewriteRule :: Circuit -> RewriteRule -> Bool -> Circuit
applyRewriteRule str rule True  = applyRewriteRuleRec str (lhs rule) (rhs rule)
applyRewriteRule str rule False = applyRewriteRuleRec str (rhs rule) (lhs rule)

-----------------------------------------------------------------------------------------
-- * RewriteOp

-- | Applies a rewrite rule at the specified position, in the specified direction.
data RewriteOp = RewriteOp { rule :: RewriteRule
                           , pos :: Int
                           , isLhsToRhs :: Bool
                           } deriving (Show,Eq)

-- | Internal implementation of checkRewriteOp. The circuit is the string to rewrite. The
-- integer is the index at which to apply the rewrite operation. The boolean flag is true
-- if the rewrite rule should be applied left-to-right. True is returned if the rule is
-- applicable.
checkRewriteOpRec :: Circuit -> Int -> RewriteRule -> Bool -> Bool
checkRewriteOpRec str 0 rule fwd = checkRewriteRule str rule fwd
checkRewriteOpRec []  _ _    _   = False
checkRewriteOpRec str n rule fwd = checkRewriteOpRec (tail str) (n - 1) rule fwd

-- | Consumes a circuit and a rewrite operation. Returns true if rule matches at the
-- position indicated by the rewrite rule.
checkRewriteOp :: Circuit -> RewriteOp -> Bool
checkRewriteOp str op = checkRewriteOpRec str (pos op) (rule op) (isLhsToRhs op)

-- | Internal implementation of applyRewriteOp. The circuit is the string to rewrite. The
-- integer is the index to apply the rewrite. The rewrite rule is the rewrite operation
-- to apply. The boolean flag is true if the rewrite rule should be applied
-- left-to-right. Returns the new circuit.
applyRewriteOpRec :: Circuit -> Int -> RewriteRule -> Bool -> Circuit
applyRewriteOpRec str 0 rule fwd = applyRewriteRule str rule fwd
applyRewriteOpRec str n rule fwd = (head str) :
                                   (applyRewriteOpRec (tail str) (n - 1) rule fwd)

-- | Consumes a circuit and a rewrite operation. Returns the string obtained by applying
-- the rewrite operation. Assumes that checkRewriteOp is true. 
applyRewriteOp :: Circuit -> RewriteOp -> Circuit
applyRewriteOp str op = applyRewriteOpRec str (pos op) (rule op) (isLhsToRhs op)
