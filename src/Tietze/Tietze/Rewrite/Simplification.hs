-- | This module provides functions to rewrite a monoidal word from a sequence of
-- rewrites.

module Tietze.Rewrite.Simplification
  ( RewriteResult (..)
  , simplify
  ) where
 
-------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Common (MonWord)
import Tietze.Rewrite.Rules
  ( Rewrite
  , applyRewrite
  , checkRewrite
  )

-----------------------------------------------------------------------------------------
-- * Rewrite Evaluation.

-- | Describes the result of a derivation sequence. States the final string obtian, the
-- step at which this string was obtained, and whether or not this corresponds to the
-- final step in the derivation sequence.
data RewriteResult = RewriteResult { output  :: MonWord
                                   , step    :: Int
                                   , success :: Bool
                                   } deriving (Show,Eq)

-- | Consumes a monoid word and a list of rewrites. Returned the string obtained by
-- performing all rewrites in order, or failure data.
simplify :: MonWord -> [Rewrite] -> RewriteResult
simplify str []     = RewriteResult str 0 True
simplify str (r:rs) = if checkRewrite str r
                      then let res = simplify (applyRewrite str r) rs
                           in RewriteResult (output res) (step res + 1) (success res)
                      else RewriteResult str 0 False
