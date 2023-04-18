-- | Internals for EIRules. Enables unit testing.

module Lafont.Edit.Internal.EIRules (
    toEDir,
    toIDir,
    asLeftDual,
    asRightDual
) where

import           Lafont.Common
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Helper methods to identify elimination/introduction rules.

-- | Consumes a rewrite rule. If the rule can be used as an elimination rule, then
-- returns the elimination direction and the introduced word. 
toEDir :: RewriteRule -> Maybe (RuleDir, MonWord)
toEDir rule
    | lhsIsNull && rhsIsNull = Nothing
    | lhsIsNull && bidir     = Just (R2L, rhs rule)
    | rhsIsNull              = Just (L2R, lhs rule)
    | otherwise              = Nothing
    where lhsIsNull = null $ lhs rule
          rhsIsNull = null $ rhs rule
          bidir     = equational rule

-- | Consumes a rewrite rule. If the rule can be used as an introduction rule, then
-- returns the elimination direction and the eliminated word.
toIDir :: RewriteRule -> Maybe (RuleDir, MonWord)
toIDir rule
    | lhsIsNull && rhsIsNull = Nothing
    | rhsIsNull && bidir     = Just (R2L, lhs rule)
    | lhsIsNull              = Just (L2R, rhs rule)
    | otherwise              = Nothing
    where lhsIsNull = null $ lhs rule
          rhsIsNull = null $ rhs rule
          bidir     = equational rule

-- | Consumes a monoidal word. If the word is not empty, then returns the word split into
-- its rightmost symbol and the remaining string. Otherwise, nothing is returned.
asLeftDual :: MonWord -> Maybe (MonWord, Symbol)
asLeftDual []     = Nothing
asLeftDual [symb] = Just ([], symb)
asLeftDual word   = Just (init word, last word)

-- | Consumes a monoidal word. If the word is not empty, then returns the word split into
-- its leftmost symbol and the remaining string. Otherwise, nothing is returned.
asRightDual :: MonWord -> Maybe (Symbol, MonWord)
asRightDual []          = Nothing
asRightDual (symb:rest) = Just (symb, rest)
