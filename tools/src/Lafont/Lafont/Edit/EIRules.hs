-- | This module provides tools to work with elimination and introduction
-- rules for monoid elements.

module Lafont.Edit.EIRules (
    -- Re-exports from internals.
    EIRule,
    -- Exports.
    EIDict,
    EIQueryType ( .. ),
    getInv,
    getEICount,
    getEIRule,
    hasLeftInv,
    queryEIRule,
    toEDict,
    toIDict
) where

import           Lafont.Maybe
import           Lafont.Common
import qualified Data.Map             as Map
import           Lafont.Edit.Internal.EIRules
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Elimination/Introduction Rule Inspection.

-- | Returns the inverse produced or consumed by an EIRule.
getInv :: EIRule -> MonWord
getInv (EIRule _ inv _ _ _) = inv

-- | Returns true if the inverse appears on the left side of the EIRule.
hasLeftInv :: EIRule -> IsLeftInv
hasLeftInv (EIRule _ _ _ lflag _) = lflag

-----------------------------------------------------------------------------------------
-- * Elimination/Introduction Construction Dictionary.

-- | Utility type to refer to the underlying map of EIDict.
type EIMap = Map.Map Symbol [EIRule]

-- | A mapping from symbols, to either their introduction or elimination rules.
data EIDict = EIDict Int EIMap deriving (Eq,Show)

-- | A function that consumes whether inverses appear on the left or right, together with
-- a dictionary of rewrite rules. A dictionary of EIRules is constructed as follows. For
-- each rewrite rule (r) in th dictionary, if r is an EIRule of the intended type, then r
-- is converted to an EIRule with inverses on the specified side, and then this rule is
-- added to the dictionary. Otherwise, the rule is ignored. The final dictionary is
-- returned.
type EIDictFn = IsLeftInv -> RuleDict -> EIDict

-- | Implementation details for toEIDict. Provides a foldRules function according to the
-- specifications of EIDictFn.
eiFold :: EIRuleFn -> IsLeftInv -> (String, RewriteRule) -> (Int, EIMap) -> (Int, EIMap)
eiFold f isLeftInv (name, rule) (sz, dict) = 
    case f isLeftInv name rule of
        Just (sym, eirule) -> (sz + 1, Map.insertWith (++) sym [eirule] dict)
        Nothing            -> (sz, dict)

-- | Implementation details for toEDict and toIDict. The EIRuleFn is the function used to
-- determine if each rewrite rule is in fact an EIRule of the correct type.
toEIDict :: EIRuleFn -> EIDictFn
toEIDict f isLeftInv rules = EIDict sz dict
    where (sz, dict) = foldRules (eiFold f isLeftInv) (0, Map.empty) rules

-- | Implements EIDictFn for elimination rules.
toEDict :: EIDictFn
toEDict = toEIDict asERule

-- | Implements EIDictFn for introduction rules.
toIDict :: EIDictFn
toIDict = toEIDict asIRule

-----------------------------------------------------------------------------------------
-- * Dictionary Access.

-- | Returns the number of rules in the EIDict.
getEICount :: EIDict -> Int
getEICount (EIDict sz _) = sz

-- | Returns all rules matching a given symbol in an EIDict.
getEIRule :: EIDict -> Symbol -> [EIRule]
getEIRule (EIDict _ dict) sym = Map.findWithDefault [] sym dict

-----------------------------------------------------------------------------------------
-- * Dictionary Query.

-- | An enumeration of EIRule query strategies:
-- 1. FirstRule: returns the first matching EIRule.
-- 2. ShortestInv: returns the unique shortest inverse, or nothing.
-- 3. MinimalInv: returns the first rule whose inverse is of minimal length.
-- 4. SelfInv: returns a self-inverse rule, or nothing.
-- 5. NoDefault: returns the unique EIRule, or nothing.
data EIQueryType = FirstRule
                 | ShortestInv
                 | MinimalInv
                 | SelfInv
                 | NoDefault
                 deriving (Show,Eq)

-- | Consumes a symbol and a list of EIRules for the symbol. Returns a rule indicating
-- that the symbol is self-inverse.
findSelfInv :: Symbol -> [EIRule] -> Maybe EIRule
findSelfInv sym []     = Nothing
findSelfInv sym (r:rs) = if isSelfInv then Just r else findSelfInv sym rs
    where isSelfInv = getInv r == [sym]

-- | Implementation details for findMinima. Consumes a list of EIRules. If the list is
-- empty, then nothing is returned. Otherwise, returns a tuple (r, l, u) where l is a
-- rule of minimal length, l is the length of the rule, and u is true if and only if u
-- is in fact the minimum rule (a unique minima).
findMinimaImpl :: [EIRule] -> Maybe (EIRule, Int, Bool)
findMinimaImpl []     = Nothing
findMinimaImpl (r:rs) =
    case findMinimaImpl rs of
        Nothing               -> Just (r, len, True)
        Just res@(_, mlen, _) -> case compare len mlen of
            LT -> Just (r, len, True)
            EQ -> Just (r, len, False)
            GT -> Just res
    where len = length $ getInv r

-- | Consumes a bool and a list of EIRules. If the list is empty, then nothing is
-- returned. Otherwise, returns the rule closest to the front of the list, whose inverse
-- is of minimal length. If the boolean flag is set, and the minimal element is not
-- unique, then nothing is returned instead.
findMinima :: Bool -> [EIRule] -> Maybe EIRule
findMinima findUnique rules =
    branchJust (findMinimaImpl rules) $ \(rule, _, isUnique) ->
        if not findUnique || isUnique
        then Just rule
        else Nothing

-- | Implementation details for queryEIRule. Requires EIRules extracted from EIDIct for
-- the specified symbol.
_queryImpl :: EIQueryType -> Symbol -> [EIRule] -> Maybe EIRule
_queryImpl _           _ []     = Nothing
_queryImpl FirstRule   _ (r:_)  = Just r
_queryImpl ShortestInv _ rules  = findMinima True rules
_queryImpl MinimalInv  _ rules  = findMinima False rules
_queryImpl SelfInv     s rules  = findSelfInv s rules
_queryImpl NoDefault   _ (r:rs) = if null rs then Just r else Nothing

-- | Consumes a dictionary of EIRules, a symbol, and a query type. If the symbol is not
-- in the dictionary, then nothing is returned. Otherwise, returns a rule according to
-- the query type. If no rule satisfies the query type, then nothing is returned.
queryEIRule :: EIDict -> Symbol -> EIQueryType -> Maybe EIRule
queryEIRule dict sym ty = _queryImpl ty sym $ getEIRule dict sym
