-- | This module provides functions to rewrite a circuit from a sequence of rewrite ops.

module DyadicRewrite.Rewrite.Simplification where

import DyadicRewrite.Common (Circuit)
import DyadicRewrite.Rewrite.Rules (RewriteOp, checkRewriteOp, applyRewriteOp)

-----------------------------------------------------------------------------------------
-- * Simplifier

data RewriteResult = RewriteResult {output :: Circuit
                                   , step :: Int
                                   , success :: Bool
                                   } deriving (Show,Eq)

simplifyRec :: Circuit -> [RewriteOp] -> Int -> RewriteResult
simplifyRec str []    step = RewriteResult str step True
simplifyRec str rules step =
    if (checkRewriteOp str (head rules))
    then (simplifyRec (applyRewriteOp str (head rules)) (tail rules) (step + 1))
    else RewriteResult str step False

simplify :: Circuit -> [RewriteOp] -> RewriteResult
simplify str rules = simplifyRec str rules 0
