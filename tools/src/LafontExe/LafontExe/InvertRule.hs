 -- | Implementation of invert_rule.

module LafontExe.InvertRule where

import qualified Data.Set             as Set
import           Lafont.Common
import           Lafont.Named
import           Lafont.Edit.EIRules
import           Lafont.Edit.Invert
import           Lafont.Parse.DerivationFile
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Derivations
import           Lafont.Rewrite.Lookup
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Simplification
import           Lafont.Rewrite.Summary
import           LafontExe.IO.Files
import           LafontExe.Logging.ErrorFormat
import           LafontExe.Logging.Graph
import           LafontExe.Logging.LineBased
import           LafontExe.Logging.Primitive
import           LafontExe.Logic.Derivations
import           LafontExe.Logic.QueryEIRules
import           LafontExe.Logic.Relations
import           System.IO

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Summarizes the command-line arguments provided as an inversion query.
data InvQuery = InvQuery String IsLeftInv EIQueryType deriving (Show, Eq)

-- | Helper method to log all EIRewrites in a derivation.
logEIRewrites :: [EIRewrite] -> String
logEIRewrites []           = ""
logEIRewrites (rule:rules) = display rule ++ "\n" ++ logEIRewrites rules

-- | Helper method to log an InversionProof as a derivation.
logProof :: String -> InversionProof -> String
logProof relname proof = header ++ ibody ++ sbody ++ ebody ++ footer
    where ibody  = logEIRewrites $ introStep proof
          sbody  = (showRewrite relname $ swapStep proof) ++ "\n"
          ebody  = logEIRewrites $ elimStep proof
          header = (logWord $ invLhs proof) ++ "\n"
          footer = (logWord $ invRhs proof) ++ "\n"

-- | Consumes a list of rules (including all derived rules), a rewrite rule, and an
-- InvQuery associated with the rewrite rule. InvQuery is satisfiable, then the resulting
-- derivation is returned in a textual representation. Otherwise, an error message is
-- returned identifying which symbol does not have satisfying EIRules.
inversion :: RuleDict -> RewriteRule -> InvQuery -> String
inversion drules rule (InvQuery relname isLeftInv ty) =
    case resolveEIQuery drules esyms isyms isLeftInv ty of
        EQueryFailure sym          -> reportMissingERule sym
        IQueryFailure sym          -> reportMissingIRule sym
        EIQuerySuccess eview iview -> case getInvProof eview iview rule of
            Just proof -> logProof relname proof
            Nothing    -> "failure (2)\n"
    where isyms = Set.fromList $ lhs rule
          esyms = Set.fromList $ rhs rule

-- | Consumes a list of rules, a list of named abstract derivations, and an InvQuery. If
-- the InvQuery is satisfiable, then the resulting derivation is returned in a textual
-- representation. Otherwise, an error message is returned identifying which symbol does
-- not have satisfying EIRules.
handleDerivedRels :: RuleDict -> [Named AbsDerivation] -> InvQuery -> String
handleDerivedRels rules named query@(InvQuery relname isLeftInv ty) =
    case interpretRule drules relname of
        Just rule -> inversion drules rule query
        Nothing   -> "failure (1)\n"
    where drules = addDRules rules $ map value named

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), a list of generators (gens), and a rule inversion request (query). If
-- all derivations parse correctly, then the specified rule is inverted and printed to
-- the handle (or a reason is given as to why the inversion is not possible). Otherwise,
-- a parsing error is printed to the handle with file name and line number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> InvQuery -> IO ()
processDerivationFiles hdl fnames rules gens query = do
    readResult <- readDerivationFiles gens fnames
    case readResult of
        Left (fname, ln, err) -> hPutStr hdl $ logEitherMsg fname ln err
        Right prederivs       -> case processPreDerivations prederivs rules gens of
            DupDeriv fname id     -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule id
            BadDeriv fname ln err -> hPutStr hdl $ logEitherMsg fname ln err
            NamedDerivs derivs    -> hPutStr hdl $ handleDerivedRels rules derivs query

-- | See doInversion. Requires that both files exist, whereas doInversion does not impose
-- this assumption.
queryEIRulesImpl :: Handle -> String -> String  -> [String] -> InvQuery -> IO ()
queryEIRulesImpl hdl genFname relFname derivFnames query = do
    genFile <- readNamedFile genFname
    relFile <- readNamedFile relFname
    case readGeneratorsAndRules genFile relFile of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname       -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname       -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair gens rules -> processDerivationFiles hdl derivFnames rules gens query

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), a list of derivation file names (derivFnames), and a rule inversion
-- request (query). If all files parse correctly, then the specified rule is inverted and
-- printed to the handle (or a reason is given as to why the inversion is not possible).
-- Otherwise, a parsing error is printed to the handle with file name and line number.
doInversion :: Handle -> String -> String -> [String] -> InvQuery -> IO ()
doInversion hdl genFname relFname derivFnames query = do
    res <- doFilesExist $ genFname:relFname:derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> queryEIRulesImpl hdl genFname relFname derivFnames query
