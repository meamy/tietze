-- | Utilities to parse and validate derivations (syntactically).

module TietzeExe.Logic.Derivations where

import           Lafont.Either
import           Lafont.Named
import           Lafont.Parse.DerivationFile
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Derivations
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Summary
import           TietzeExe.IO.Files
import           TietzeExe.Logging.ErrorFormat
import           TietzeExe.Logging.Graph

-----------------------------------------------------------------------------------------
-- * Type-specialized parsing of derivations.

-- | Outcome of parsing a list of derivations relative to a rule dictionary. The results
-- (DupDeriv fname ln) and (BadDerivFile fname ln err) indicate errors encountered when
-- parsing derivation files. The result (NamedDerivs derivs) indicates a success.
data DerivReadResult = DupDeriv String Int
                     | BadDeriv String Int DFPError
                     | NamedDerivs [Named AbsDerivation]

-- Consumes a dictionary of rewrite rules (rules) and a list of named PreDerivations. If
-- each PreDerivation summary is either unnamed or has a unqiue name (with respect to the
-- relations and other PreDerivations), then a set of PreDerivation summary names is
-- returned. Otherwise, the file name of the first PreDerivation with a duplicate summary
-- name is returned.
listDerivedRules :: RuleDict -> [Named PreDerivation] -> Either (String, Int) DRuleSet
listDerivedRules _     []           = Right nullRuleSet
listDerivedRules rules (named:rest) =
    case listDerivedRules rules rest of
        Left err  -> Left err
        Right set -> case addSummaryToSymbols rules set summary of
            Nothing   -> Left (source named, identifier named)
            Just set' -> Right set'
    where summary = parsed $ value named

-- | Consumes a dictionary of rewrite rules (rules) and a list of pairs, where each pair
-- contains the name of a file and the PreDerivation data it describes. If all files
-- parse correctly, then returns a list of pairs, where each pair contains the name of a
-- pair and the Derivation it describes. Otherwise, a parsing error is returned. Requires
-- that all derived rules have already been recorded in rules
parseRewriteSections :: RuleDict -> DRuleSet -> [Named PreDerivation] 
                                 -> ParseFilesRV DFPError AbsDerivation
parseRewriteSections _     _       []                         = Right []
parseRewriteSections rules derived ((Named src idx pre):rest) =
    case parseDerivationFile rules derived pre of
        Left (ln, err) -> Left (src, ln, err)
        Right deriv    -> updateRight (parseRewriteSections rules derived rest) $ \v ->
            Named src idx deriv : v

-- | Consumes a list of named PreDerivations (prederivs), a rule dictionary (rules), and
-- a list of generators. If parsing is succesful, then returns a set of derived rules,
-- together with a list of derivations. Otherwise, returns an error explaining why the
-- derivation parsing failed.
processPreDerivations :: [Named PreDerivation] -> RuleDict -> [String] -> DerivReadResult
processPreDerivations prederivs rules gens =
    case listDerivedRules rules prederivs of
        Left (fname, id) -> DupDeriv fname id
        Right derived    -> case parseRewriteSections rules derived prederivs of
            Left (fname, ln, err) -> BadDeriv fname ln err
            Right derivations     -> NamedDerivs derivations

-- | Implmentation details for concretize.
cimpl :: DerivationMetadata -> [Named AbsDerivation] -> ParseFilesRV Int Derivation
cimpl _    []                                 = Right []
cimpl meta ((Named src idx derv):derivations) =
    case concretizeDerivation meta derv of
        Left pos    -> Left (src, idx, pos)
        Right deriv -> updateRight (cimpl meta derivations) $ \rest ->
            Named src idx deriv : rest

-- | Takes as input a list of named derivations. If the derivations are not free from
-- errors, then a textual representation of the first encountered error is returned (see
-- detectDerivationError). Otherwise, if a derivation fails to be concretized, then a
-- textual representation of why this derivation cannot be concretized is returned.
-- Otherwise, all derivations are concretized and the corresponding concrete derivations
-- are returned, with the same named metadata.
concretize :: [Named AbsDerivation] -> Either String [Named Derivation]
concretize named =
    case detectDerivationError absDerivations of
        Just (Left dep) -> Left $ "Unmet dependency: " ++ printUnmetDep dep ++ "\n"
        Just (Right c)  -> Left $ "Dependency cycle detected: " ++ printCycle c ++ "\n"
        Nothing         -> case cimpl (dmap, emap) named of
            Left (fname, num, pos) -> Left $ describeFailedApply fname num Nothing pos
            Right derivs           -> Right derivs
    where absDerivations = Prelude.map value named
          dmap           = makeDerivationMap absDerivations
          emap           = identifyEquationalRules dmap
