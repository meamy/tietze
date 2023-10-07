-- | Internals for EIRules. Enables unit testing.

module Lafont.Edit.Internal.EIRules (
    EIRule ( .. ),
    EIRuleFn,
    IsLeftInv,
    toEDir,
    toIDir,
    asLeftInv,
    asRightInv,
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
-- requirements of a specific EIRule type, then the direction and non-empty side of the
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
asLeftInv :: MonWord -> Maybe (Symbol, MonWord)
asLeftInv []     = Nothing
asLeftInv [symb] = Just (symb, [])
asLeftInv word   = Just (last word, init word)

-- | Consumes a monoidal word. If the word is not empty, then returns the word split into
-- its leftmost symbol and the remaining string. Otherwise, nothing is returned.
asRightInv :: MonWord -> Maybe (Symbol, MonWord)
asRightInv []          = Nothing
asRightInv (symb:rest) = Just (symb, rest)

-----------------------------------------------------------------------------------------
-- * Representation of an elimination or introduction (co-elimination) rule.

-- | Indicates if the rule is derived from other relations. In practice, this indicates
-- if the apply keyword is required.
type IsDerived = Bool

-- | Indicates if the inverse string should appear on the left.
type IsLeftInv = Bool

-- | Provides an elimination (or introduction) interface to a rewrite rule. The first
-- argument is the name of the relation. The second argument is the inverse word involved
-- in the relation (any additional symbols introduced or eliminated in the process). The
-- fourth argument is the direction the rule must be applied in to act as intended. The
-- fourth argument indicates is the rule is derive d and therefore must be applied.
data EIRule = EIRule String MonWord RuleDir IsLeftInv IsDerived deriving (Show,Eq)

-----------------------------------------------------------------------------------------
-- * Helper methods to extract elimination/introduction rules.

-- | Consumes a flag that indicates whether the inverse word in an elimination rule
-- should appear on the left or right. Returns a function f that consumes both the name
-- of a rewrite rule, and the rule itself. If the rule is of the correct EI type, then f
-- returns the symbol to eliminate/introduce, and the details of the
-- elimination/introduction. Otherwise, nothing is returned.
type EIRuleFn = IsLeftInv -> String -> RewriteRule -> Maybe (Symbol, EIRule)

-- | Implementation details for asERule and asIRule. The EIDirFn is the function used to
-- determine the direction (and consequently the type) of the relation.
asEIRule :: EIDirFn -> EIRuleFn
asEIRule f isLeftInv relname rel =
    branchJust (f rel) $ \(dir, res) ->
        branchJust (asInv res) $ \(mor, inv) ->
            Just (mor, EIRule relname inv dir isLeftInv isDerived)
    where asInv     = if isLeftInv then asLeftInv else asRightInv
          isDerived = isDerivedRule rel

-- | Implements EIRuleFn for elimination rules.
asERule :: EIRuleFn
asERule = asEIRule toEDir

-- | Implements EIRuleFn for introduction rules.
asIRule :: EIRuleFn
asIRule = asEIRule toIDir
