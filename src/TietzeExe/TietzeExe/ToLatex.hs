-- | Implementation of to_latex.

module TietzeExe.ToLatex (toLatex) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List.NonEmpty
  ( NonEmpty
  , toList
  )
import Tietze.Named (Named (..))
import Tietze.Format.Common (formatDerivation)
import Tietze.Format.LaTeX
  ( MacroList
  , makeGenMacros
  , makeRelMacros
  , printFormattedProof
  , printMacroList
  )
import Tietze.Rewrite.Abstraction
  ( AbsDerivation
  , addDRules
  )
import Tietze.Rewrite.Derivations (Derivation (..))
import Tietze.Rewrite.Lookup (RuleDict)
import Tietze.Rewrite.Summary
  ( DerivationSummary (..)
  , RewritePreamble (..)
  )
import TietzeExe.IO.Files
  ( doFilesExist
  , readDerivationFiles
  , readNamedFile
  , readNamedFiles
  )
import TietzeExe.Logging.ErrorFormat
  ( reportDupRule
  , reportInvalidRule
  , reportUnknownGen
  )
import TietzeExe.Logging.LineBased
  ( logEitherMsg
  , logFromFile
  )
import TietzeExe.Logic.Derivations
  ( DerivReadResult (..)
  , concretize
  , processPreDerivations
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

-- | Generates a label for each derivation.
derivationToLabel :: Named Derivation -> String
derivationToLabel (Named src idx (Derivation sum _)) =
    case propName $ meta sum of
        Just name -> "% Derivation: " ++ name
        Nothing   -> "% From File: " ++ src ++ "(" ++ show idx ++ ")"

-- | Converts all derivations to LaTeX environments.
handleDerivations :: MacroList -> MacroList -> [Named Derivation] -> String
handleDerivations _       _       []        = ""
handleDerivations gmacros rmacros (d:dlist) = label ++ "\n" ++ latex ++ "\n\n" ++ rtext
    where label = derivationToLabel d
          proof = formatDerivation $ value d
          latex = printFormattedProof gmacros rmacros proof
          rtext = handleDerivations gmacros rmacros dlist

-- | Implementation details for generateLatex. Assumes that all derivations can be
-- concretized, and are taken as an argument.
generateLatexImpl :: RuleDict -> [String] -> [Named Derivation] -> String
generateLatexImpl rules gens named = glatex ++ "\n\n" ++ rlatex ++ "\n\n" ++ dlatex
    where gmacros = makeGenMacros gens
          rmacros = makeRelMacros rules
          glatex  = printMacroList gmacros
          rlatex  = printMacroList rmacros
          dlatex  = handleDerivations gmacros rmacros named

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the list of
-- derivations is invalid, then the error is printed. Otherwise, the derivations are
-- converted to a LaTeX representation and printed to the handle.
generateLatex :: RuleDict -> [String] -> [Named AbsDerivation] -> String
generateLatex rules gens named =
    case concretize named of
        Left errmsg  -> errmsg
        Right named' -> generateLatexImpl drules gens named'
    where drules = addDRules rules $ Prelude.map value named

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the the derivations are converted to a LaTeX representation and printed to the
-- handle. Otherwise, a parsing error is printed to the handle with file name and line
-- number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> IO ()
processDerivationFiles hdl fnames rules gens = do
    readResult <- readDerivationFiles gens fnames
    case readResult of
        Left (fname, ln, err) -> hPutStr hdl $ logEitherMsg fname ln err
        Right prederivs       -> case processPreDerivations prederivs rules gens of
            DupDeriv fname id     -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule id
            BadDeriv fname ln err -> hPutStr hdl $ logEitherMsg fname ln err
            NamedDerivs derivs    -> hPutStr hdl $ generateLatex rules gens derivs

-- | See toLatex. Requires that both files exist, whereas toLatex does not impose this
-- assumption.
validateDerivationsImpl :: Handle -> String -> [String] -> [String] -> IO ()
validateDerivationsImpl hdl genFname relFnames derivFnames = do
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
-- correctly, then the derivations are converted to a LaTeX representation. Otherwise, a
-- parsing error is printed to the handle with file name and line number.
toLatex :: Handle -> String -> NonEmpty String -> [String] -> IO ()
toLatex hdl genFname relFnames derivFnames = do
    res <- doFilesExist $ genFnames' ++ relFnames' ++ derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> validateDerivationsImpl hdl genFname relFnames' derivFnames
    where relFnames' = toList relFnames
          genFnames' = [genFname]
