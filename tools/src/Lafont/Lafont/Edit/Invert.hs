-- | This module provides tools to invert a rewrite rule through a combination
-- of elimination and introduction rules.

module Lafont.Edit.Invert (
    EIRewrite ( .. ),
    EIView,
    addRule,
    createView,
    deriveElim,
    deriveIntro,
    invertRule,
    findRule,
    hasLeftInvs
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
-- single elimination (resp. introduction) rule for each symbol. In addition, each EIView
-- enforces that all inverses appear strictly on either the left or right.
data EIView = EIView IsLeftInv (Map.Map Symbol EIRule) deriving (Eq, Show)

-- | Initializes a new EIView in which no symbol has elimination (resp. introduction)
-- rule. The addRule method is used to assign rules to symbols.
createView :: IsLeftInv -> EIView
createView usingLeftInv = EIView usingLeftInv Map.empty

-- | Consumes an EIView, and a pair consisting of a symbol and an EIRule. Returns a new
-- view in which the given symbol is associated with the given EIRule, and all other
-- mappings remain unchanged.
addRule :: EIView -> (Symbol, EIRule) -> Maybe EIView
addRule (EIView usingLeftInv map) (sym, rule) =
    if usingLeftInv == hasLeftInv rule
    then Just $ EIView usingLeftInv $ Map.insert sym rule map
    else Nothing

-- | Consumes an EIView and a symbol. If the symbol is associated with an EIRule, then
-- returns the EIRule. Otherwise, nothing is returned.
findRule :: EIView -> Symbol -> Maybe EIRule
findRule (EIView _ map) sym = Map.lookup sym map

-- | Returns true if an EIVIew contains rules using left inverses.
hasLeftInvs :: EIView -> IsLeftInv
hasLeftInvs (EIView usingLeftInv _) = usingLeftInv

-----------------------------------------------------------------------------------------
-- * Functions to Invert Rewrite Rules.

-- | Implementation details for invertStr. Excepts that the word is reversed, so that
-- the resulting string appears in a contravariant order.
invertStrImpl :: EIView -> MonWord -> Maybe MonWord
invertStrImpl _     []        = Just []
invertStrImpl view (sym:word) =
    branchJust (findRule view sym) $ \rule ->
        branchJust (invertStrImpl view word) $ \dword ->
            Just $ getInv rule ++ dword

-- | Implements invertRule for a single side of a RewriteRule. Note that the invert of
-- the lhs and rhs only differ in the choice of view.
invertStr :: EIView -> MonWord -> Maybe MonWord
invertStr view word = invertStrImpl view $ reverse word

-- | Consumes an EIView for elimination rules, an EIView for introduction rules, and an
-- arbitrary rewrite rule. If a symbol appears in the rewrite rule which does not appear
-- in the corresponding EIView (lhs is introduced, rhs is eliminated), then nothing is
-- returned. Otherwise, returns inverted relation.
invertRule :: EIView -> EIView -> RewriteRule -> Maybe (MonWord, MonWord)
invertRule eview iview rule =
    branchJust (invertStr eview $ rhs rule) $ \lstr ->
        branchJust (invertStr iview $ lhs rule) $ \rstr -> Just (lstr, rstr)

-----------------------------------------------------------------------------------------
-- * Functions to Derive Inverse Rewrite Rules.

-- | A variation of the Rewrite type to EIRules.
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
-- as input a view of in-scope elimination rules, the current index, and the word to
-- eliminate at this index. Whether inverses appear on the left or right is inferred from
-- the view. If the elimination is possible, then returns a sequence of eliminations,
-- together with the new index after the sequence of eliminations. Otherwise, nothing is
-- returned. Note that derivation only fails if a elim rule is missing.
deriveElim :: EIView -> Int -> MonWord -> Maybe (Int, [EIRewrite])
deriveElim view n word = seqToDer delta view n seq
    where linvs = hasLeftInvs view
          seq    = if linvs then word else reverse word
          delta  = if linvs then negate . length . getInv else const (- 1)

-- | This function expands introduction rules from symbols to words. The function takes
-- as input a view of in-scope introduction rules, the current index, and the word to
-- introduce at this index. Whether inverses appear on the left or right is inferred from
-- the view. If the introduction is possible, then returns a sequence of introductions,
-- together with the new index after the sequence of introduction. Otherwise, nothing is
-- returned. Note that derivation only fails if an intro rule is missing.
deriveIntro :: EIView -> Int -> MonWord -> Maybe (Int, [EIRewrite])
deriveIntro view n word = seqToDer delta view n seq
    where linvs = hasLeftInvs view
          seq    = if linvs then reverse word else word
          delta  = if linvs then length . getInv else const 1
