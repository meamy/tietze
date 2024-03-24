-- | Utilities to query EIRules given sets of symbols.

module TietzeExe.Logic.QueryEIRules where

import qualified Data.Set             as Set
import           Lafont.Common
import           Lafont.Edit.EIRules
import           Lafont.Edit.Invert
import           Lafont.Rewrite.Lookup

-----------------------------------------------------------------------------------------
-- * Type-specialized relation processing.

-- | Summarizes the results of resolveEIQuery. If both the elimination and introduction
-- queries could be satisfied, then returns two EIViews, where the first contains all
-- elimination rules, and the second contains all introduction rules. Otherwise, returns
-- an indication of where the first failure occured (either elimination or introduction)
-- together with the rule which could not be satisfied.
data EIQueryRes = EQueryFailure Symbol
                | IQueryFailure Symbol
                | EIQuerySuccess EIView EIView

-- | Consumes a rule dictionary, two sets of symbols, a flag indicating whether inverses
-- (for elimination rules) should appear on the left, and an EIRule query type. Attempts
-- to construct an EIView of elimination rules for the first set of symbols, and an
-- EIView of introduction rules for the second set of symbols, with inverses appearing on
-- the correct side. The EIRules are selected from the provided rule dictionary,
-- according to the provided query type. The outcome of this function is returned
-- according to EIQueryRes.
resolveEIQuery :: RuleDict -> Set.Set Symbol -> Set.Set Symbol
                           -> IsLeftInv -> EIQueryType -> EIQueryRes
resolveEIQuery rules esyms isyms isLeftInv ty =
    case viewByQuery esyms ty edict of
        QueryFailure sym   -> EQueryFailure sym
        QuerySuccess eview -> case viewByQuery isyms ty idict of
            QueryFailure sym   -> IQueryFailure sym
            QuerySuccess iview -> EIQuerySuccess eview iview
    where edict = toEDict isLeftInv rules
          idict = toIDict (not isLeftInv) rules
