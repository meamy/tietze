-- | This module provides tools to dualize a rewrite rule through a combination
-- of elimination and introduction rules.

module Lafont.Edit.Dualize (
    EIRewrite ( .. ),
    EIView,
    addRule,
    createView,
    deriveElim,
    deriveIntro,
    dualizeRule,
    findRule
) where

import qualified Data.Map             as Map
import           Lafont.Common
import           Lafont.Maybe
import           Lafont.Edit.EIRules
import           Lafont.Edit.Internal.EIRules
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Data-Types to Specify a Selection of EIRules

-- | A single symbol might have multiple elimination (resp. introduction) rules. Even if
-- all elimination (resp. introduction) rules are equivalent up to isomorphism (e.g., the
-- rules encode group inverses), this choice of syntactic representation still impacts
-- the structure of a derivational proof. An EIView solves this problem by selecting a
-- single elimination (resp. introduction) rule for each symbol.
newtype EIView = EIView (Map.Map Symbol EIRule)

-- | Initializes a new EIView in which no symbol has elimination (resp. introduction)
-- rule. The addRule method is used to assign rules to symbols.
createView :: EIView
createView = EIView Map.empty

-- | Consumes an EIView, and a pair consisting of a symbol and an EIRule. Returns a new
-- view in which the given symbol is associated with the given EIRule, and all other
-- mappings remain unchanged.
addRule :: EIView -> (Symbol, EIRule) -> EIView
addRule (EIView map) (sym, rule) = EIView $ Map.insert sym rule map

-- | Consumes an EIView and a symbol. If the symbol is associated with an EIRule, then
-- returns the EIRule. Otherwise, nothing is returned.
findRule :: EIView -> Symbol -> Maybe EIRule
findRule (EIView map) sym = Map.lookup sym map

-----------------------------------------------------------------------------------------
-- * Functions to Dualize Rewrite Rules.

-- | Implementation details for dualizeStr. Excepts that the word is reversed, so that
-- the resulting string appears in a contravariant order.
dualizeStrImpl :: EIView -> MonWord -> Maybe MonWord
dualizeStrImpl _     []         = Just []
dualizeStrImpl view (sym:word) =
    branchJust (findRule view sym) $ \rule ->
        branchJust (dualizeStrImpl view word) $ \dword ->
            Just $ getDual rule ++ dword

-- | Implements dualizeRule for a single side of a RewriteRule. Note that the dualize of
-- the lhs and rhs only differ in the choice of view.
dualizeStr :: EIView -> MonWord -> Maybe MonWord
dualizeStr view word = dualizeStrImpl view $ reverse word

-- | Consumes an EIView for elimination rules, an EIView for introduction rules, and an
-- arbitrary rewrite rule. If a symbol appears in the rewrite rule which does not appear
-- in the corresponding EIView (lhs is introduced, rhs is eliminated), then nothing is
-- returned. Otherwise, returns dual relation.
dualizeRule :: EIView -> EIView -> RewriteRule -> Maybe (MonWord, MonWord)
dualizeRule eview iview rule =
    branchJust (dualizeStr eview $ rhs rule) $ \lstr ->
        branchJust (dualizeStr iview $ lhs rule) $ \rstr -> Just (lstr, rstr)

-----------------------------------------------------------------------------------------
-- * Functions to Derive Dual Rewrite Rules.

-- | 
data EIRewrite = EIRewrite Int EIRule deriving (Show,Eq)

-- | Helper method to implement derivElim and deriveIntro. The additional function from
-- EIRule to integer is used to track the current index in the derivation. Specifically,
-- for each EIRule in the derivation, the function should return the number of positions
-- by which the rule moves the current index.
seqToDer :: (EIRule -> Int) -> EIView -> Int -> MonWord -> Maybe (Int, [EIRewrite])
seqToDer _     _    n []         = Just (n, [])
seqToDer delta view n (sym:word) =
    branchJust (findRule view sym) $ \rule ->
        let n' = n + delta rule
        in branchJust (seqToDer delta view n' word) $ \(end, deriv) ->
            Just (end, EIRewrite n rule:deriv)

-- | This function expands elimination rules from symbols to words. The function takes
-- as input a flag indicating if duals appear on the left, a view of the elimination
-- rules in scope, the current index, and the word to eliminate at this index. If the
-- elimination is possible, then returns a sequence of eliminations, together with the
-- new index after the sequence of eliminations. Otherwise, nothing is returned.
deriveElim :: IsLeftDual -> EIView -> Int -> MonWord -> Maybe (Int, [EIRewrite])
deriveElim True  view n word = seqToDer (negate . length . getDual) view n word
deriveElim False view n word = seqToDer (\_ -> (-1))                view n $ reverse word

-- | This function expands introduction rules from symbols to words. The function takes
-- as input a flag indicating if duals appear on the left, a view of the introduction
-- rules in scope, the current index, and the word to eliminate at this index. If the
-- introduction is possible, then returns a sequence of introductions, together with the
-- new index after the sequence of introductions. Otherwise, nothing is returned.
deriveIntro :: IsLeftDual -> EIView -> Int -> MonWord -> Maybe (Int, [EIRewrite])
deriveIntro True  view n word = seqToDer (length . getDual) view n $ reverse word
deriveIntro False view n word = seqToDer (\_ -> 1)          view n word
