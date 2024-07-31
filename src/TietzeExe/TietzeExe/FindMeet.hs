-- | Implementation of find_meet.

module TietzeExe.FindMeet
  ( DerQuery (..)
  , doFindMeet
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
  , findMeet
  , newDeriveLog
  , rulesToOptions
  )
import Tietze.Rewrite.Common (RuleDir(..))
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

-- | Helper function to log the word at which the left-hand and right-hand sides meet.
logMeet :: Handle -> MonWord -> IO ()
logMeet hdl meet = hPutStr hdl $ logWord meet ++ "\n"

-- | Helper function to perform an unbounded, iterative meet search.
execute :: Handle -> DeriveLog -> DeriveLog -> [DOption] -> MonWord -> MonWord -> [Int]
                  -> IO ()
execute hdl llog rlog _    _   _   []      = hPutStr hdl "Search failed."
execute hdl llog rlog opts lhs rhs (i:itr) = do
    hPutStr hdl $ "Beginning Iteration: " ++ show i ++ "\n"
    case findMeet L2R llog opts rlog lhs i of
        Right meet -> logMeet hdl meet
        Left nllog -> case findMeet R2L rlog opts nllog rhs i of
            Right meet -> logMeet hdl meet
            Left nrlog -> execute hdl nllog nrlog opts lhs rhs itr

-- | Helper function to set up search parameters, from which a meet search is performed.
setup :: Handle -> RuleDict -> DerQuery -> IO ()
setup hdl rules (DerQuery name itrs cutoff) =
    case interpretRule rules name of
        Nothing   -> hPutStr hdl $ "Cannot find rule: " ++ name ++ "\n"
        Just rule -> execute hdl dlog dlog opts (rhs rule) (lhs rule) itrs
    where opts = rulesToOptions rules $ Set.fromList [name]
          dlog = newDeriveLog cutoff

-- | See doFindMeet. Requires that both files exist, whereas doFindMeet does not impose
-- this assumption.
doFindMeetImpl :: Handle -> String -> [String] -> DerQuery -> IO ()
doFindMeetImpl hdl genFname relFnames query = do
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
-- parse correctly, then a search is performed to find a word at which the left-hand and
-- right-hand sides meet. Upon success, the word is logged to the handle. Otherwise, a
-- parsing error is printed to handle with the file name and line number.
--
-- Note: This is a semi-decision procedure. If a proof does not exist, then this function
-- will never terminate.
doFindMeet :: Handle -> String -> NonEmpty String -> DerQuery -> IO ()
doFindMeet hdl genFname relFnames query = do
    res <- doFilesExist $ genFnames' ++ relFnames'
    case res of
        Just name -> putStrLn $ "File does not exist: " ++ name
        Nothing   -> doFindMeetImpl hdl genFname relFnames' query
    where relFnames' = toList relFnames
          genFnames' = [genFname]
