 -- | Implementation of query_eirules.

module TietzeExe.QueryEIRules
  ( EIQuery (..)
  , queryEIRules
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Data.List.NonEmpty
  ( NonEmpty
  , toList
  )
import qualified Data.Set as Set
import Tietze.Common (Symbol)
import Tietze.Named (Named (..))
import Tietze.Edit.EIRules
  ( EIQueryType
  , IsLeftInv
  )
import Tietze.Edit.Invert (EIView)
import Tietze.Rewrite.Abstraction
  ( AbsDerivation
  , addDRules
  )
import Tietze.Rewrite.Lookup (RuleDict)
import TietzeExe.IO.Files
  ( doFilesExist
  , readDerivationFiles
  , readNamedFile
  , readNamedFiles
  )
import TietzeExe.Logging.ErrorFormat
  ( reportDupRule
  , reportInvalidRule
  , reportMissingERule
  , reportMissingIRule
  , reportUnknownGen
  )
import TietzeExe.Logging.LineBased
  ( logEitherMsg
  , logFromFile
  )
import TietzeExe.Logging.Primitive (logEIView)
import TietzeExe.Logic.Derivations
  ( DerivReadResult (..)
  , processPreDerivations
  )
import TietzeExe.Logic.QueryEIRules
  ( EIQueryRes (..)
  , resolveEIQuery
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

-- | Summarizes the command-line arguments provided as an EI rule query.
data EIQuery = EIQuery (Set.Set Symbol) IsLeftInv EIQueryType deriving (Show, Eq)

-- | Consumes a set of symbols, an elimination EIView, and an introduction EIView, under
-- the assumption that all symbols appear in both views. Returns a string which contains
-- a line for each elimination and introduction rule associated with symbol.
logQueryResults :: Set.Set Symbol -> EIView -> EIView -> String
logQueryResults symset eview iview = estr ++ "\n" ++ istr
    where estr = "[Elimination Rules]\n" ++ logEIView symset eview
          istr = "[Introduction Rules]\n" ++ logEIView symset eview

-- | Consumes a list of rules, a list of named abstract derivations, and an EIQuery. If
-- the EIQuery is satisfiable, then the satisfying EIRules are returned in a textual
-- representation. Otherwise, an error message is returned identifying which symbol does
-- not have satisfying EIRules.
handleDerivedRels :: RuleDict -> [Named AbsDerivation] -> EIQuery -> String
handleDerivedRels rules named (EIQuery symset isLeftInv ty) =
    case resolveEIQuery drules symset symset isLeftInv ty of
        EQueryFailure sym          -> reportMissingERule sym
        IQueryFailure sym          -> reportMissingIRule sym
        EIQuerySuccess eview iview -> logQueryResults symset eview iview
    where drules = addDRules rules $ Prelude.map value named

-- | Consumes a handle, a list of derivation files (DerivFnames), a dictionary of rewrite
-- rules (rules), a list of generators (gens). and the configurations for an EI view
-- (query). If all derivations parse correctly, then the specified view is generated and
-- printed to the handle (or a reason is given as to why no such view exists). Otherwise,
-- a parsing error is printed to the handle with file name and line number.
processDerivationFiles :: Handle -> [String] -> RuleDict -> [String] -> EIQuery -> IO ()
processDerivationFiles hdl fnames rules gens query = do
    readResult <- readDerivationFiles gens fnames
    case readResult of
        Left (fname, ln, err) -> hPutStr hdl $ logEitherMsg fname ln err
        Right prederivs       -> case processPreDerivations prederivs rules gens of
            DupDeriv fname id     -> hPutStr hdl $ logFromFile fname 0 $ reportDupRule id
            BadDeriv fname ln err -> hPutStr hdl $ logEitherMsg fname ln err
            NamedDerivs derivs    -> hPutStr hdl $ handleDerivedRels rules derivs query

-- | See queryEIRules. Requires that both files exist, whereas queryEIRules does not
-- impose this assumption.
queryEIRulesImpl :: Handle -> String -> [String] -> [String] -> EIQuery -> IO ()
queryEIRulesImpl hdl genFname relFnames derivFnames query = do
    genFile  <- readNamedFile genFname
    relFiles <- readNamedFiles relFnames
    case readGeneratorsAndRules genFile relFiles of
        UnknownSem             -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err   -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname       -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname       -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair gens rules -> processDerivationFiles hdl derivFnames rules gens query

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), a list of derivation file names (derivFnames), and the configurations
-- for an EI view (query). If all files parse correctly, then the specified view is
-- generated and printed to the handle (or a reason is given as to why no such view
-- exists). Otherwise, a parsing error is printed to the handle with file name and line
-- number.
queryEIRules :: Handle -> String -> NonEmpty String -> [String] -> EIQuery -> IO ()
queryEIRules hdl genFname relFnames derivFnames query = do
    res <- doFilesExist $ genFnames' ++ relFnames' ++ derivFnames
    case res of
        Just name -> hPutStr hdl $ "File does not exist: " ++ name ++ "\n"
        Nothing   -> queryEIRulesImpl hdl genFname relFnames' derivFnames query
    where relFnames' = toList relFnames
          genFnames' = [genFname]
