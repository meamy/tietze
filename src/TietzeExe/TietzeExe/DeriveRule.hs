-- | Implementation of derive_rule.

module TietzeExe.DeriveRule
  ( DerQuery (..)
  , doDerivation
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Set as Set

import Data.List.NonEmpty
  ( NonEmpty
  , toList
  )
import Tietze.Common (MonWord)
import Tietze.Edit.Derive
  ( DeriveLog
  , DOption
  , deriveRule
  , newDeriveLog
  , rulesToOptions
  )
import Tietze.Rewrite.Lookup
  ( RuleDict
  , interpretRule
  )
import Tietze.Rewrite.Rules
  ( Rewrite
  , RewriteRule (..)
  , showRewrite
  )
import TietzeExe.IO.Files
  ( doFilesExist
  , readNamedFile
  , readNamedFiles
  )
import TietzeExe.Logging.ErrorFormat
  ( reportInvalidRule
  , reportUnknownGen
  )
import TietzeExe.Logging.LineBased (logEitherMsg)
import TietzeExe.Logging.Primitive (logWord)
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

-- | Summarizes the command-line arguments provided as an inversion query.
data DerQuery = DerQuery String [Int] Int deriving (Show, Eq)

-- | Helper function to log the sequence of rule applications in a derivation.
logSteps :: [(String, Rewrite)] -> String
logSteps []               = ""
logSteps ((name, rw):rws) = showRewrite name rw ++ "\n" ++ logSteps rws

-- | Helper function to log a derivation.
logDerivation :: MonWord -> [(String, Rewrite)] -> MonWord -> String
logDerivation lhs rws rhs = head ++ "\n" ++ body ++ foot ++ "\n"
    where head = logWord lhs
          body = logSteps rws
          foot = logWord rhs

-- | Helper function to perform an unbounded, iterative proof search.
execute :: Handle -> DeriveLog -> [DOption] -> MonWord -> MonWord -> [Int] -> IO ()
execute hdl log _    _    _    []      = hPutStr hdl "Search failed."
execute hdl log opts goal init (i:itr) = do
    hPutStr hdl $ "Beginning Iteration: " ++ show i ++ "\n"
    case deriveRule log opts goal init i of
        Left nlog -> execute hdl nlog opts goal init itr
        Right rws -> hPutStr hdl $ logDerivation init rws goal

-- | Helper function to set up search parameters, from which a proof search is performed.
setup :: Handle -> RuleDict -> DerQuery -> IO ()
setup hdl rules (DerQuery name itrs cutoff) =
    case interpretRule rules name of
        Nothing   -> hPutStr hdl $ "Cannot find rule: " ++ name ++ "\n"
        Just rule -> execute hdl dlog opts (rhs rule) (lhs rule) itrs
    where opts = rulesToOptions rules $ Set.fromList [name]
          dlog = newDeriveLog cutoff

-- | See doDerivation. Requires that both files exist, whereas doDerivation does not
-- impose this assumption.
doDerivationImpl :: Handle -> String -> [String] -> DerQuery -> IO ()
doDerivationImpl hdl genFname relFnames query = do
    genFile  <- readNamedFile genFname
    relFiles <- readNamedFiles relFnames
    case readGeneratorsAndRules genFile relFiles of
        UnknownSem           -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname     -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname     -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair _ rules  -> setup hdl rules query

-- | Consumes a handle, the name of a generator file (genFname), the name of a relation
-- file (relFname), and a derivational proof query. If the generator and relation files
-- parse correctly, then a proof search is performed based on the proof query, with the
-- proof logged to the handle upon success. Otherwise, a parsing error is printed to
-- handle with the file name and line number.
--
-- Note: This is a semi-decision procedure. If a proof does not exist, then this function
-- will never terminate.
doDerivation :: Handle -> String -> NonEmpty String -> DerQuery -> IO ()
doDerivation hdl genFname relFnames query = do
    res <- doFilesExist $ genFnames' ++ relFnames'
    case res of
        Just name -> putStrLn $ "File does not exist: " ++ name
        Nothing   -> doDerivationImpl hdl genFname relFnames' query
    where relFnames' = toList relFnames
          genFnames' = [genFname]
