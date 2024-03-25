-- | Implementation of check_relations.

module TietzeExe.GraphDeps (graphDeps) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List.NonEmpty
  ( NonEmpty
  , toList
  )
import Lafont.Named (Named (..))
import Lafont.Format.GraphViz
  ( DotFile
  , printDotFile
  )
import Lafont.Rewrite.Abstraction (AbsDerivation)
import Lafont.Rewrite.Lookup (RuleDict)
import TietzeExe.IO.Files
  ( doFilesExist
  , readDerivationFiles
  , readNamedFile
  , readNamedFiles
  )
import TietzeExe.IO.Configs (Style (..))
import TietzeExe.Logging.ErrorFormat
  ( reportDupRule
  , reportInvalidRule
  , reportUnknownGen
  )
import TietzeExe.Logging.Graph (printUnmetDep)
import TietzeExe.Logging.LineBased
  ( logEitherMsg
  , logFromFile
  )
import TietzeExe.Logic.Derivations
  ( DerivReadResult (..)
  , processPreDerivations
  )
import TietzeExe.Logic.GraphDeps
  ( AnnotatedDotFile
  , applyNodeColour
  , derivationsToDotFile
  , unwrapDotFile
  )
import TietzeExe.Logic.Relations
  ( GenRuleReadResult (..)
  , readGeneratorsAndRules
  )
import System.IO
  ( Handle
  , hPutStr
  )

-----------------------------------------------------------------------------------------
-- * Logic.

-- | Applies a style file to an annotated DotFile. The underlying DotFile is returned.
styleDotFile :: Style -> AnnotatedDotFile -> DotFile
styleDotFile sty adot = unwrapDotFile $ applyNodeColour (color sty) adot

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the derivations is
-- well-defined (though possibly cyclic), then the dependency graph is printed as a DOT
-- file. Otherwise, an error describing the unmet dependency is printed.
generateDepGraph :: Style -> [String] -> [Named AbsDerivation] -> String
generateDepGraph sty types named =
    case derivationsToDotFile types abs of
        Left unmet -> "Unmet dependency: " ++ printUnmetDep unmet ++ "\n"
        Right adot -> (printDotFile $ styleDotFile sty adot) ++ "\n"
    where abs = Prelude.map value named

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the dependency graph is generated and printed as a DOT file. Otherwise, a parsing
-- error is printed to the handle with file name and line number.
processDerivFiles :: Handle -> Style -> [String] -> [String] -> RuleDict -> [String] -> IO ()
processDerivFiles hdl sty types fnames rules gens = do
    readResult <- readDerivationFiles gens fnames
    case readResult of
        Left (fname, ln, err) -> hPutStr hdl $ logEitherMsg fname ln err
        Right prederivs       -> case processPreDerivations prederivs rules gens of
            DupDeriv fname id     -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule id
            BadDeriv fname ln err -> hPutStr hdl $ logEitherMsg fname ln err
            NamedDerivs derivs    -> hPutStr hdl $ generateDepGraph sty types derivs

-- | See graphDeps. Requires that both files exist, whereas graphDeps does not impose
-- this assumption.
graphDepsImpl :: Handle -> Style -> [String] -> String -> [String] -> [String] -> IO ()
graphDepsImpl hdl sty types genFname relFnames derivFnames = do
    genFile  <- readNamedFile genFname
    relFiles <- readNamedFiles relFnames
    case readGeneratorsAndRules genFile relFiles of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname       -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname       -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair gens rules -> processDerivFiles hdl sty types derivFnames rules gens

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), and list of derivation file names (derivFnames). If all files parse
-- correctly, then the derivations are printed as the DOT file for their dependency
-- graph. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
graphDeps :: Handle -> Style -> [String] -> String -> NonEmpty String -> [String] -> IO ()
graphDeps hdl sty types genFname relFnames derivFnames = do
    res <- doFilesExist $ genFnames' ++ relFnames' ++ derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> graphDepsImpl hdl sty types genFname relFnames' derivFnames
    where relFnames' = toList relFnames
          genFnames' = [genFname]
