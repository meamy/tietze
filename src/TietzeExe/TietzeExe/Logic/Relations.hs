-- | Utilities to parse and validate relations.

module TietzeExe.Logic.Relations
  ( GenRuleReadResult (..)
  , readGeneratorsAndRules
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Lafont.Generators.Categories (MonoidObj)
import Lafont.Generators.RuleSem
  ( RuleDictStatus (..)
  , checkRuleSem
  )
import Lafont.Generators.Semantics
  ( GenDict
  , toAlphabet
  )
import Lafont.Parse.GeneratorFile
  ( GenFileSummary (..)
  , GFPError
  , parseGenFileAsDict
  )
import Lafont.Parse.RelationFile
  ( RFPError
  , parseRelFile
  )
import Lafont.Rewrite.Lookup
  ( RuleDict (..)
  , empty
  )
import TietzeExe.IO.Files (FileData (..))

-----------------------------------------------------------------------------------------
-- * Type-specialized parsing and validation of generators and relations.

-- | Outcome of parsing a relation file relative to a generator file. The result
-- UnknownSem should never be seen in practice, and indicates that a semantic model lacks
-- an type specialization. The results (BadGenFile fn ln err) and (BadRelFile fn ln err)
-- are used to propogate a parsing error err from file fn at line ln. The results
-- (InvalidRule id) and (MissingGen id) indicates a semantic error in the rule named id.
-- The result (GenRulePair gens rules) indicates a success.
data GenRuleReadResult = UnknownSem
                       | BadGenFile String Int GFPError
                       | BadRelFile String Int RFPError
                       | InvalidRel String
                       | MissingGen String
                       | GenRulePair [String] RuleDict

-- | Recursive implementation of readRules over the entries of the file list.
readRulesImpl :: [String] -> [FileData] -> RuleDict -> GenRuleReadResult
readRulesImpl language []                            rules = GenRulePair language rules
readRulesImpl language ((FileData name lines):files) rules =
    case parseRelFile rules language lines 1 of
        Left (errLn, err) -> BadRelFile name errLn err
        Right rules'      -> readRulesImpl language files rules'

-- | Consumes a list of relation files and a dictionary of generators. If all relation
-- files are parsed successfully, relative to the generators, then a GenRulePair status
-- is returned. Otherwise, and error is propogated via the return status BadRelFile.
--
-- Note: This method is said to be unchecked, as semantic validity is ignored.
readRules :: GenDict a -> [FileData] -> GenRuleReadResult
readRules gens files = readRulesImpl language files Lafont.Rewrite.Lookup.empty
    where language = toAlphabet gens

-- | Consumes a relation file and a dictionary of generators whose semantics are given by
-- elements of a monoid. This function adheres to readRules, except that the semantics
-- validity of each rules is also checked. The following two cases are introduced. If a
-- rule is semantically invalid, then the InvalidRule status is returned. If the
-- semantics of a generator are undefined, then the MissingGen status is returned.
readAndCheckRules :: (MonoidObj a) => GenDict a -> [FileData] -> GenRuleReadResult
readAndCheckRules gens files =
    case readRules gens files of
        GenRulePair language rules -> case checkRuleSem gens rules of
            InvalidRuleSem id   -> InvalidRel id
            IncompleteGenSet id -> MissingGen id
            GoodRuleDict        -> GenRulePair language rules
        badResult -> badResult

-- | Consumes a generator file and a relation file. If the generator fail is invalid,
-- then the BadGenFile status is returned. Otherwise, the rules are parsed relative to
-- the generators, in adherence to readAndCheckRules.
readGeneratorsAndRules :: FileData -> [FileData] -> GenRuleReadResult
readGeneratorsAndRules (FileData genFname genLines) relFiles =
    case parseGenFileAsDict genLines 1 of
        Left (errLn, err)                    -> BadGenFile genFname errLn err
        Right (MonoidGenSummary dict)        -> readRules dict relFiles
        Right (DyadicTwoSummary dict)        -> readAndCheckRules dict relFiles
        Right (DyadicThreeSummary dict)      -> readAndCheckRules dict relFiles
        Right (ModMultProductSummary dict _) -> readAndCheckRules dict relFiles
        Right (ModAddProductSummary dict _)  -> readAndCheckRules dict relFiles
        _                                    -> UnknownSem
