-- | This module provides types and functions for string rewrite rules. In particular
-- RewriteRules rewrite the prefix of a string whereas RewriteOps apply RewriteRules to a
-- specified substring of a string.

module DyadicRewrite.Rewrite.Rules where

import DyadicRewrite.Common (Circuit)

-----------------------------------------------------------------------------------------
-- * RewriteRule

-- | Describes a rewrite rule of the form: lhs => rhs or lhs <=> rhs (equational). Also
-- indicates if the rule was derived from other relations.
data RewriteRule = RewriteRule { lhs        :: Circuit
                               , rhs        :: Circuit
                               , equational :: Bool
                               , derived    :: Bool
                               } deriving (Show,Eq)

-- | Consumes a circuit and the lhs of a production rule: lhs → rhs. True is returned if
-- the rule is applicable at index .
doesRewriteTermMatch :: Circuit -> Circuit -> Bool
doesRewriteTermMatch _   []  = True
doesRewriteTermMatch []  _   = False
doesRewriteTermMatch str lhs = if (head str) == (head lhs)
                               then doesRewriteTermMatch (tail str) (tail lhs)
                               else False

-- | Consumes a circuit, a rewrite rule, and a boolean flag indicating if the rule is to
-- be applied from left-to-right. Returns true if rule matches a prefix of the circuit.
checkRewriteRule :: Circuit -> RewriteRule -> Bool -> Bool
checkRewriteRule str rule True  = doesRewriteTermMatch str (lhs rule)
checkRewriteRule str rule False = doesRewriteTermMatch str (rhs rule)

-- | Internal implementation of applyRewriteRule. The first circuit is the string to
-- rewrite. The second circuit is the lhs of the rewrite rule. The second circuit is the
-- rhs of the rewrite rule. Returns the new circuit.

-- | Consumes a circuit, together with the lhs and rhs of a production rule: lhs → rhs.
-- Returns the circuit obtained by applying the production rule at index.  Assumes that
-- doesRewriteTermMatch is true.
applyProductionRule :: Circuit -> Circuit -> Circuit -> Circuit
applyProductionRule str []  []  = str
applyProductionRule str []  rhs = (head rhs) : (applyProductionRule str [] (tail rhs))
applyProductionRule str lhs rhs = applyProductionRule (tail str) (tail lhs) rhs

-- | Consumes a circuit, a rewrite rule, and a boolean flag indicating if the rule is to
-- be applied from left-to-right. Returns the string obtained by applying the rewrite
-- rule. Assumes that checkRewriteRule is true. 
applyRewriteRule :: Circuit -> RewriteRule -> Bool -> Circuit
applyRewriteRule str rule True  = applyProductionRule str (lhs rule) (rhs rule)
applyRewriteRule str rule False = applyProductionRule str (rhs rule) (lhs rule)

-----------------------------------------------------------------------------------------
-- * RewriteOp

-- | Applies a rewrite rule at the specified position, in the specified direction.
data RewriteOp = RewriteOp { rule :: RewriteRule
                           , pos :: Int
                           , isLhsToRhs :: Bool
                           } deriving (Show,Eq)

-- | Consumes a circuit and a rewrite operation. Returns true if rule matches at the
-- position indicated by the rewrite rule.
checkRewriteOp :: Circuit -> RewriteOp -> Bool
checkRewriteOp str op = impl str (pos op)
    where impl substr n = if n == 0
                          then checkRewriteRule substr (rule op) (isLhsToRhs op)
                          else if substr == []
                               then False
                               else impl (tail substr) (n - 1)

-- | Consumes a circuit and a rewrite operation. Returns the string obtained by applying
-- the rewrite operation. Assumes that checkRewriteOp is true. 
applyRewriteOp :: Circuit -> RewriteOp -> Circuit
applyRewriteOp str op = impl str (pos op)
    where impl substr n = if n == 0
                          then applyRewriteRule substr (rule op) (isLhsToRhs op)
                          else (head substr) : (impl (tail substr) (n - 1))
