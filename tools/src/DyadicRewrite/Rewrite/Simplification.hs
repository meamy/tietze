-- | This module provides functions to rewrite a circuit from a sequence of rewrite ops.

module DyadicRewrite.Rewrite.Simplification where

import DyadicRewrite.Common (Circuit)
import DyadicRewrite.Rewrite.Rules (RewriteOp, checkRewriteOp, applyRewriteOp)

-----------------------------------------------------------------------------------------
-- * Simplifier

data RewriteResult = RewriteResult { output :: Circuit
                                   , step :: Int
                                   , success :: Bool
                                   } deriving (Show,Eq)

-- | Consumes a circuit and a list of rewrite operations. Returned the string obtained
-- by executing all rewrite operations, or failure data.
simplify :: Circuit -> [RewriteOp] -> RewriteResult
simplify str []    = RewriteResult str 0 True
simplify str rules = if (checkRewriteOp str rule)
                     then let res = (simplify (applyRewriteOp str rule) (tail rules))
                          in RewriteResult (output res) ((step res) + 1) (success res)
                     else RewriteResult str 0 False
    where rule = (head rules)
