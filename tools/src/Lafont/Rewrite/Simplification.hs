-- | This module provides functions to rewrite a monoidal word from a sequence of
-- rewrites.

module Lafont.Rewrite.Simplification where

import Lafont.Common
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Rewrite Evaluation.

data RewriteResult = RewriteResult { output :: MonWord
                                   , step :: Int
                                   , success :: Bool
                                   } deriving (Show,Eq)

-- | Consumes a monoid word and a list of rewrites. Returned the string obtained by
-- performing all rewrites in order, or failure data.
simplify :: MonWord -> [Rewrite] -> RewriteResult
simplify str []    = RewriteResult str 0 True
simplify str rules = if (checkRewrite str rule)
                     then let res = (simplify (applyRewrite str rule) (tail rules))
                          in RewriteResult (output res) ((step res) + 1) (success res)
                     else RewriteResult str 0 False
    where rule = (head rules)
