-- | Implementation of check_relations.

module TietzeExe.CheckRelations (checkRelations) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import Data.List.NonEmpty
  ( NonEmpty
  , toList
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
import TietzeExe.Logging.Primitive (logRuleDict)
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

-- | See checkRelations. Requires that both files exist, whereas checkRelations does not
-- impose this assumption.
checkRelationsImpl :: Handle -> String -> [String] -> IO ()
checkRelationsImpl hdl genFname relFnames = do
    genFile  <- readNamedFile genFname
    relFiles <- readNamedFiles relFnames
    case readGeneratorsAndRules genFile relFiles of
        UnknownSem           -> hPutStr hdl "Impl Error: Unknown semantic model."
        BadGenFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        BadRelFile fn ln err -> hPutStr hdl $ logEitherMsg fn ln err
        InvalidRel rname     -> hPutStr hdl $ reportInvalidRule rname
        MissingGen rname     -> hPutStr hdl $ reportUnknownGen rname
        GenRulePair _ rules  -> hPutStr hdl $ logRuleDict rules

-- | Consumes a handle, the name of a generator file (genFname) and the name of a
-- relation file (relFname). If the generator and relation files parse correctly, then an
-- internal representation of the generators is printed to handle. Otherwise, a parsing
-- error is printed to handle with the file name and line number.
checkRelations :: Handle -> String -> NonEmpty String -> IO ()
checkRelations hdl genFname relFnames = do
    res <- doFilesExist $ genFnames' ++ relFnames'
    case res of
        Just name -> putStrLn ("File does not exist: " ++ name)
        Nothing   -> checkRelationsImpl hdl genFname relFnames'
    where relFnames' = toList relFnames
          genFnames' = [genFname]
