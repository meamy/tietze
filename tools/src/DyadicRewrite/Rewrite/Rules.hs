-- | This module provides types and functions for string rewrite rules. In particular
-- RewriteRules rewrite the prefix of a string whereas RewriteOps apply RewriteRules to a
-- specified substring of a string.

module DyadicRewrite.Rewrite.Rules where

type Circuit = [String]

-----------------------------------------------------------------------------------------
-- * RewriteRule

-- | Describes a rewrite rule of the form: lhs <=> rhs.
data RewriteRule = RewriteRule { lhs :: Circuit, rhs :: Circuit }

-- | Internal implementation of checkRewriteRule. The first circuit is the string to
-- rewrite. The second circuit is the lhs of the rewrite rule. True if returned if the
-- rule is applicable.
checkRewriteRuleRec :: Circuit -> Circuit -> Bool
checkRewriteRuleRec str []  = True
checkRewriteRuleRec []  lhs = False
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
applyRewriteRule :: Circuit -> RewriteRule -> Bool -> [String]
applyRewriteRule str rule True  = applyRewriteRuleRec str (lhs rule) (rhs rule)
applyRewriteRule str rule False = applyRewriteRuleRec str (rhs rule) (lhs rule)
