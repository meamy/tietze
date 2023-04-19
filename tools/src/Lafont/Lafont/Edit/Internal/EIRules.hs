-- | Internals for EIRules. Enables unit testing.

module Lafont.Edit.Internal.EIRules (
    EIRule ( .. ),
    toEDir,
    toIDir,
    asLeftDual,
    asRightDual,
    asERule,
    asIRule
) where

import           Lafont.Common
import           Lafont.Maybe
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Helper methods to identify elimination/introduction rules.

-- | A function that consumes a rewrite rule. If the rewrite rule satisfies the
-- requirements of a specifici EIRule type, then the direction and non-empty side of the
-- relation are returned. Otherwise, nothing is returned.
type EIDirFn = RewriteRule -> Maybe (RuleDir, MonWord)

-- | Consumes a rewrite rule. If the rule can be used as an elimination rule, then
-- returns the elimination direction and the eliminated word. 
toEDir :: EIDirFn
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
toIDir :: EIDirFn
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
asLeftDual :: MonWord -> Maybe (Symbol, MonWord)
asLeftDual []     = Nothing
asLeftDual [symb] = Just (symb, [])
asLeftDual word   = Just (last word, init word)

-- | Consumes a monoidal word. If the word is not empty, then returns the word split into
-- its leftmost symbol and the remaining string. Otherwise, nothing is returned.
asRightDual :: MonWord -> Maybe (Symbol, MonWord)
asRightDual []          = Nothing
asRightDual (symb:rest) = Just (symb, rest)

-----------------------------------------------------------------------------------------
-- * Representation of an elimination or introduction (co-elimination) rule.

-- | Indicates if the rule is derived from other relations. In practice, this indicates
-- if the apply keyword is required.
type IsDerived = Bool

-- | Provides an elimination (or introduction) interface to a rewrite rule. The first
-- argument is the name of the relation. The second argument is the dual object involved
-- in the relation (any additional symbols introduced or eliminated in the process. The
-- fourth argument is the direction the rule must be applied in to act as intended. The
-- fourth argument indicates is the rule is derive d and therefore must be applied.
data EIRule = EIRule String MonWord RuleDir IsDerived deriving (Show,Eq)

-----------------------------------------------------------------------------------------
-- * Helper methods to extract elimination/introduction rules.

-- | Indicates if the dual string should appear on the left.
type IsLeftDual = Bool

-- | Implementation details for asERule and asIRule. The EIDirFn is the function used to
-- determine the direction (and consequently the type) of the relation.
asEIRule :: EIDirFn -> IsLeftDual -> String -> RewriteRule -> Maybe (Symbol, EIRule)
asEIRule f isLeftDual relname rel =
    branchJust (f rel) $ \(dir, res) ->
        branchJust (asDual res) $ \(mor, dual) ->
            Just (mor, EIRule relname dual dir isDerived)
    where asDual    = if isLeftDual then asLeftDual else asRightDual
          isDerived = isDerivedRule rel

-- | Consumes a flag that indicates whether the dualizing object in an elimination rule
-- should appear on the left or right. Returns a function f that consumes both the name
-- of a rewrite rule, and the rule itself. If the rule performs an elimination, then f
-- returns the symbol to eliminate, and the details of the elimination. Otherwise,
-- nothing is returned.
asERule :: IsLeftDual -> String -> RewriteRule -> Maybe (Symbol, EIRule)
asERule = asEIRule toEDir

-- | Consumes a flag that indicates whether the dualizing object in an introduction rule
-- should appear on the left or right. Returns a function f that consumes both the name
-- of a rewrite rule, and the rule itself. If the rule performs an introduction, then f
-- returns the symbol which is introduced, and the details of the introduction.
-- Otherwise, nothing is returned.
asIRule :: IsLeftDual -> String -> RewriteRule -> Maybe (Symbol, EIRule)
asIRule = asEIRule toIDir
