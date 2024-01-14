-- | Implementation of check_relations.

module LafontExe.GraphDeps where

import           Data.List.NonEmpty
import           Lafont.Named
import           Lafont.Format.GraphViz
import           Lafont.Parse.DerivationFile
import           Lafont.Rewrite.Abstraction
import           Lafont.Rewrite.Lookup
import           LafontExe.IO.Files
import           LafontExe.Logging.ErrorFormat
import           LafontExe.Logging.Graph
import           LafontExe.Logging.LineBased
import           LafontExe.Logic.Derivations
import           LafontExe.Logic.Relations
import           System.IO

-----------------------------------------------------------------------------------------
-- * Logic.

-- | See generateDepGraph.
generateDepGraphImpl :: DepGraph -> String
generateDepGraphImpl deps = printDotFile $ graphToDotFile g unsafeToNodeID
    where g = unwrapDepGraph deps

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the derivations is
-- well-defined (though possibly cyclic), then the dependency graph is printed as a DOT
-- file. Otherwise, an error describing the unmet dependency is printed.
generateDepGraph :: [Named AbsDerivation] -> String
generateDepGraph named =
    case derivationToGraph absDerivations of
        Left unmet -> "Unmet dependency: " ++ printUnmetDep unmet ++ "\n"
        Right g    -> generateDepGraphImpl g ++ "\n"
    where absDerivations = Prelude.map value named

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the dependency graph is generated and printed as a DOT file. Otherwise, a parsing
-- error is printed to the handle with file name and line number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles hdl fnames rules gens = do
    readResult <- readDerivationFiles gens fnames
    case readResult of
        Left (fname, ln, err) -> hPutStr hdl $ logEitherMsg fname ln err
        Right prederivs       -> case processPreDerivations prederivs rules gens of
            DupDeriv fname id     -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule id
            BadDeriv fname ln err -> hPutStr hdl $ logEitherMsg fname ln err
            NamedDerivs derivs    -> hPutStr hdl $ generateDepGraph derivs

-- | See graphDeps. Requires that both files exist, whereas graphDeps does not impose
-- this assumption.
graphDepsImpl :: Handle -> String -> [String] -> [String] -> IO ()
graphDepsImpl hdl genFname relFnames derivFnames = do
    genFile  <- readNamedFile genFname
    relFiles <- readNamedFiles relFnames
    case readGeneratorsAndRules genFile relFiles of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname       -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname       -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair gens rules -> processDerivationFiles hdl derivFnames rules gens

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), and list of derivation file names (derivFnames). If all files parse
-- correctly, then the derivations are printed as the DOT file for their dependency
-- graph. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
graphDeps :: Handle -> String -> NonEmpty String -> [String] -> IO ()
graphDeps hdl genFname relFnames derivFnames = do
    res <- doFilesExist $ genFnames' ++ relFnames' ++ derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> graphDepsImpl hdl genFname relFnames' derivFnames
    where relFnames' = toList relFnames
          genFnames' = [genFname]
