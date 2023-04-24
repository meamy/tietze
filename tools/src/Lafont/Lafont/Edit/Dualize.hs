-- | This module provides tools to dualize a rewrite rule through a combination
-- of elimination and introduction rules.

module Lafont.Edit.Dualize (
    EIView,
    addRule,
    createView,
    dualizeRule,
    findRule
) where

import qualified Data.Map             as Map
import           Lafont.Common
import           Lafont.Maybe
import           Lafont.Edit.EIRules
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
dualizeStrImpl model (sym:word) =
    branchJust (findRule model sym) $ \rule ->
        branchJust (dualizeStrImpl model word) $ \dword ->
            Just $ getDual rule ++ dword

-- | Implements dualizeRule for a single side of a RewriteRule. Note that the dualize of
-- the lhs and rhs only differ in the choice of view.
dualizeStr :: EIView -> MonWord -> Maybe MonWord
dualizeStr model word = dualizeStrImpl model $ reverse word

-- | Consumes an EIView for elimination rules, an EIView for introduction rules, and an
-- arbitrary rewrite rule. If a symbol appears in the rewrite rule which does not appear
-- in the corresponding EIView (lhs is introduced, rhs is eliminated), then nothing is
-- returned. Otherwise, returns dual relation.
dualizeRule :: EIView -> EIView -> RewriteRule -> Maybe (MonWord, MonWord)
dualizeRule emodel imodel rule =
    branchJust (dualizeStr emodel $ rhs rule) $ \lstr ->
        branchJust (dualizeStr imodel $ lhs rule) $ \rstr -> Just (lstr, rstr)
