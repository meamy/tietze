-- | This module provides tools to work with elimination and introduction
-- rules for monoid elements.

module Lafont.Edit.EIRules (
    -- Re-exports from internals.
    EIRule,
    -- Exports.
    EIDict,
    getEICount,
    getEIRule,
    toEDict,
    toIDict
) where

import           Lafont.Common
import qualified Data.Map             as Map
import           Lafont.Edit.Internal.EIRules
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Elimination/Introduction Construction Dictionary.

-- | Utility type to refer to the underlying map of EIDict.
type EIMap = Map.Map Symbol [EIRule]

-- | A mapping from symbols, to either their introduction or elimination rules.
data EIDict = EIDict Int EIMap deriving (Eq,Show)

-- | A function that consumes the side duals must appear on, together with a dictionary
-- of rewrite rules. A dictionary of EIRules is constructed as follows. For each rewrite
-- rule (r) in th dictionary, if r is an EIRule of the intended type, then r is converted
-- to an EIRule with duals on the specified side, and then this rule is added to the
-- dictionary. Otherwise, the rule is ignored. The final dictionary is returned.
type EIDictFn = IsLeftDual -> RuleDict -> EIDict

-- | Implementation details for toEIDict. Provides a foldRules function according to the
-- specifications of EIDictFn.
eiFold :: EIRuleFn -> IsLeftDual -> (String, RewriteRule) -> (Int, EIMap) -> (Int, EIMap)
eiFold f isLeftDual (name, rule) (sz, dict) = 
    case f isLeftDual name rule of
        Just (sym, eirule) -> (sz + 1, Map.insertWith (++) sym [eirule] dict)
        Nothing            -> (sz, dict)

-- | Implementation details for toEDict and toIDict. The EIRuleFn is the function used to
-- determine if each rewrite rule is in fact an EIRule of the correct type.
toEIDict :: EIRuleFn -> EIDictFn
toEIDict f isLeftDual rules = EIDict sz dict
    where (sz, dict) = foldRules (eiFold f isLeftDual) (0, Map.empty) rules

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
