-- | Implementation of validate_derivation.

module TietzeExe.ValidateDerivations (validateDerivations) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List.NonEmpty
  ( NonEmpty
  , toList
  )
import Tietze.Named (Named (..))
import Tietze.Rewrite.Abstraction (AbsDerivation)
import Tietze.Rewrite.Derivations (Derivation (..))
import Tietze.Rewrite.Lookup (RuleDict)
import Tietze.Rewrite.Simplification
  ( RewriteResult (..)
  , simplify
  )
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
  ( describeIncorrectResult
  , describeIncorrectStep
  , reportDupRule
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

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If a derivation is invalid, then a summary of the
-- failure is printed. Otherwise, a success message is printed.
verifyDerivationSteps :: [Named Derivation] -> String
verifyDerivationSteps []                                  = "Success.\n"
verifyDerivationSteps ((Named src idx deriv):derivations) =
    if success res
    then if output res == final summary
         then verifyDerivationSteps derivations
         else describeIncorrectResult src idx dname (final summary) (output res)
    else describeIncorrectStep src idx dname (output res) (step res)
    where Derivation summary rewrites = deriv
          res                         = simplify (initial summary) rewrites
          dname                       = propName $ meta summary

-- | Consumes a list of pairs, where each tuple contains the name of a derivation file
-- and the Derivation it describes. If the dependency graph induced by the list of
-- derivations is invalid, then the error is printed. Otherwise, if a step in a
-- derivation is invalid, then a summary of the failure is printed. Otherwise, a success
-- message is printed.
verifyDerivations :: [Named AbsDerivation] -> String
verifyDerivations named =
    case concretize named of
        Left errmsg  -> errmsg
        Right derivs -> verifyDerivationSteps derivs

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), and a list of generators (gens). If all derivations parse correctly,
-- then the derivations are validated and any invalid derivations are printed to the
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
            NamedDerivs derivs    -> hPutStr hdl $ verifyDerivations derivs

-- | See validateDerivations. Requires that both files exist, whereas validateDerivations
-- does not impose this assumption.
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
-- correctly, then the derivations are validated and any invalid derivations are printed
-- to the handle. Otherwise, a parsing error is printed to the handle with file name and
-- line number.
validateDerivations :: Handle -> String -> NonEmpty String -> [String] -> IO ()
validateDerivations hdl genFname relFnames derivFnames = do
    res <- doFilesExist $ genFnames' ++ relFnames' ++ derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> validateDerivationsImpl hdl genFname relFnames' derivFnames
    where relFnames' = toList relFnames
          genFnames' = [genFname]
