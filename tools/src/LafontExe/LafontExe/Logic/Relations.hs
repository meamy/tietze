-- |

module LafontExe.Logic.Relations where

import           Lafont.Generators.Categories
import           Lafont.Generators.RuleSem
import           Lafont.Generators.Semantics
import           Lafont.Parse.GeneratorFile
import           Lafont.Parse.RelationFile
import           Lafont.Rewrite.Lookup
import           LafontExe.IO.Files

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

-- | Consumes a relation file and a dictionary of generators. If the relation file is
-- parsed successfully, relative to the generators, then a GenRulePair status is returned.
-- Otherwise, and error is propogated via the return status BadRelFile.
--
-- Note: This method is said to be unchecked, as semantic validity is ignored.
readRulesUnchecked :: FileData -> GenDict a -> GenRuleReadResult
readRulesUnchecked (FileData relFname relLines) gens =
    case parseRelFile language relLines 0 of
        Left (errLn, err) -> BadRelFile relFname errLn err
        Right rules       -> GenRulePair language rules
    where language = toAlphabet gens

-- | Consumes a relation file and a dictionary of generators whose semantics are given by
-- elements of a monoid. This function adheres to readRulesUnchecked, except that the
-- semantics validity of each rules is also checked. The following two cases are
-- introduced. If a rule is semantically invalid, then the InvalidRule status is
-- returned. If the semantics of a generator are undefined, then the MissingGen status is
-- returned.
readAndCheckRules :: (MonoidObj a) => FileData -> GenDict a -> GenRuleReadResult
readAndCheckRules relFile gens =
    case readRulesUnchecked relFile gens of
        GenRulePair language rules -> case checkRuleSem gens rules of
            InvalidRuleSem id   -> InvalidRel id
            IncompleteGenSet id -> MissingGen id
            GoodRuleDict        -> GenRulePair language rules
        badResult -> badResult

-- | Consumes a generator file and a relation file. If the generator fail is invalid,
-- then the BadGenFile status is returned. Otherwise, the rules are parsed relative to
-- the generators, in adherence to readAndCheckRules.
readGeneratorsAndRules :: FileData -> FileData -> GenRuleReadResult
readGeneratorsAndRules (FileData genFname genLines) relFile =
    case parseGenFileAsDict genLines 0 of
        Left (errLn, err)               -> BadGenFile genFname errLn err
        Right (MonoidGenSummary dict)   -> readRulesUnchecked relFile dict
        Right (DyadicTwoSummary dict)   -> readAndCheckRules relFile dict
        Right (DyadicThreeSummary dict) -> readAndCheckRules relFile dict
        _                               -> UnknownSem
