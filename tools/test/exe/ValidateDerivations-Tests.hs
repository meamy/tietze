module Main where

import qualified Data.List.NonEmpty as NonEmpty
import           Data.List
import           LafontExeTest.HandleTest
import           LafontExe.ValidateDerivations
import           Lafont.String
import           Lafont.Rewrite.Derivations
import           System.IO

-----------------------------------------------------------------------------------------
-- Common files.

goodGens :: FilePath
goodGens = "data/test/example.gens" 

goodRels :: FilePath
goodRels = "data/test/example.rels"

goodProofOne :: FilePath
goodProofOne = "data/test/example.1.derivs"

goodProofTwo :: FilePath
goodProofTwo = "data/test/example.2.derivs"

badProofOne :: FilePath
badProofOne = "data/test/bad.1.derivs"

trivialProof :: String
trivialProof = "data/test/trivial.derivs"

-----------------------------------------------------------------------------------------
-- Utilities.

doTest :: String -> String -> [String] -> Handle -> IO ()
doTest genFname relFname derivs x = validateDerivations x genFname relFname' derivs
    where relFname' = NonEmpty.fromList [relFname]

-----------------------------------------------------------------------------------------
-- Helper methods.

isFailure :: String -> Bool
isFailure str = or [("Failed" `isSubstrOf` str),
                    ("failed" `isSubstrOf` str)]

isDuplicated :: String -> Bool
isDuplicated str = or [("Duplicate" `isSubstrOf` str),
                       ("duplicate" `isSubstrOf` str)]

isExpecting :: String -> String -> Bool
isExpecting str exp = or [(("Expected " ++ exp) `isSubstrOf` str),
                          (("expected " ++ exp) `isSubstrOf` str)]

isProducing :: String -> String -> Bool
isProducing str act = or [(("Produced " ++ act) `isSubstrOf` str),
                          (("produced " ++ act) `isSubstrOf` str)]

-----------------------------------------------------------------------------------------
-- Good input, single derivation.

test1 = (HandleTest "Good_Derivation"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (doTest goodGens goodRels [goodProofOne])
                    (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Good input, multiple derivation.

multipleGoodProofs :: [FilePath]
multipleGoodProofs = [goodProofOne, goodProofTwo]

test2 = (HandleTest "Multiple_Good_Derivation"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (doTest goodGens goodRels multipleGoodProofs)
                    (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Bad generators.

badGens :: FilePath
badGens = "data/test/bad.gens"

check3 :: String -> Bool
check3 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/bad.gens:6" `isSubstrOf` str),
                   -- The expected error.
                   ("invalid symbol" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test3 = (HandleTest "Bad_Generators"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (doTest badGens goodRels [goodProofOne])
                    check3)

-----------------------------------------------------------------------------------------
-- Bad relations.

badRels :: FilePath
badRels = "data/test/bad.rels"

check4 :: String -> Bool
check4 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.rels:5" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("xyz3" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test4 = (HandleTest "Bad_Relations"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (doTest goodGens badRels [goodProofOne])
                    check4)

-----------------------------------------------------------------------------------------
-- Bad derivation step.

check5 :: String -> Bool
check5 str =
    if ((length outputLines) == 2)
    then (and [-- The error and its location.
               (isFailure (outputLines !! 0)),
               ("data/test/bad.1.deriv" `isSubstrOf` (outputLines !! 0)),
               -- The error details.
               ("abc1.abc1.lmnop123.abc1.abc1" `isSubstrOf` (outputLines !! 1)),
               ("step 1" `isSubstrOf` (outputLines !! 1))])
    else False
    where outputLines = lines str

test5 = (HandleTest "Bad_Derivation_Step"
                    "Ensures that validateDerivations rejects invalid steps."
                    (doTest goodGens goodRels [badProofOne])
                    check5)

-----------------------------------------------------------------------------------------
-- Bad derivation result.

badProofTwo :: FilePath
badProofTwo = "data/test/bad.2.derivs"

check6 :: String -> Bool
check6 str =
    if ((length outputLines) == 2)
    then (and [-- The error and its location.
               (isFailure (outputLines !! 0)),
               ("data/test/bad.2.derivs" `isSubstrOf` (outputLines !! 0)),
               -- The error details.
               ((outputLines !! 1) `isExpecting` "abc1.abc1"),
               ((outputLines !! 1) `isProducing` "ε")])
    else False
    where outputLines = lines str

test6 = (HandleTest "Bad_Derivation_Output"
                    "Ensures that validateDerivations rejects unexpected results."
                    (doTest goodGens goodRels [badProofTwo])
                    check6)

-----------------------------------------------------------------------------------------
-- Bad derivation input.

badProofThree :: FilePath
badProofThree = "data/test/bad.3.derivs"

check7 :: String -> Bool
check7 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.3.derivs:7" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("abc5" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test7 = (HandleTest "Bad_Derivation_Format"
                    "Ensures that validateDerivations rejects unexpected results."
                    (doTest goodGens goodRels [badProofThree])
                    check7)

-----------------------------------------------------------------------------------------
-- Bad derivation in list.

mixedProofs :: [FilePath]
mixedProofs = [goodProofOne, badProofOne, goodProofTwo]

test8 = (HandleTest "Bad_Derivation_Mixed"
                    "Ensures that validateDerivations detects bad derivation in a list."
                    (doTest goodGens goodRels mixedProofs)
                    check5)

-----------------------------------------------------------------------------------------
-- Tests support for derived rules.

derivedRuleProof :: String
derivedRuleProof = "data/test/summary.derivs"

derivedProofDeps = [goodProofOne, derivedRuleProof, goodProofTwo]

check8 :: String -> Bool
check8 str = (and [-- The line at which the error should occur (see summary.derivs).
                   ("data/test/summary.derivs:10" `isSubstrOf` str),
                   -- The missing relation name.
                   ("DerivedRel1" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test9 = (HandleTest "Derived_Rules_Fail"
                    "Ensures that validateDerivations identifies derived rules (1/2)."
                    (doTest goodGens goodRels [derivedRuleProof])
                    check8)

test10 = (HandleTest "Derived_Rules_Pass"
                     "Ensures that validateDerivations identifies derived rules (2/2)."
                     (doTest goodGens goodRels derivedProofDeps)
                     (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Tests that duplicate relation names are rejected.

dupNameProof :: String
dupNameProof = "data/test/example.3.derivs"

twoProofsOneName = [dupNameProof, goodProofOne]

check11 :: String -> Bool
check11 str = (and [-- Defaults to the first line
                    ("data/test/example.3.derivs:0" `isSubstrOf` str),
                    -- Keyword in error.
                    (isDuplicated str),
                    ("index 1" `isSubstrOf` str),
                    -- Should be a single line.
                    ((length (lines str)) == 1)])

test11 = (HandleTest "Duplicate_Rel_Name"
                     "Ensures that validateDerivations identifies name collisions."
                     (doTest goodGens goodRels twoProofsOneName)
                     check11)

-----------------------------------------------------------------------------------------
-- Tests that a trivial, unammed proof passes all of the checks.

test12 = (HandleTest "Trivial_Unnamed"
                     "Ensures that validateDerivations handles trivial unammed proofs."
                     (doTest goodGens goodRels [trivialProof])
                     (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Tests cycle detection.

cycleRel1 :: String
cycleRel1 = "data/test/cycle/rel.1.derivs"

cycleRel2 :: String
cycleRel2 = "data/test/cycle/rel.2.derivs"

cycleRel3 :: String
cycleRel3 = "data/test/cycle/rel.3.derivs"

cycleRel4 :: String
cycleRel4 = "data/test/cycle/rel.4.derivs"

cycleRel5a :: String
cycleRel5a = "data/test/cycle/rel.5.a.derivs"

cycleRel5b :: String
cycleRel5b = "data/test/cycle/rel.5.b.derivs"

cyclicReasoningBad = [cycleRel1, cycleRel2, cycleRel3, cycleRel4, cycleRel5a]
cyclicReasoningFix = [cycleRel1, cycleRel2, cycleRel3, cycleRel4, cycleRel5b]

check13 :: String -> Bool
check13 str = (and [-- The line at which the error should occur (see bad.rels).
                    ("cycle detected" `isSubstrOf` str),
                    -- The sequence of vertices in the cycle.
                    ("1. Rel1" `isSubstrOf` str),
                    ("2. Rel2" `isSubstrOf` str),
                    ("3. Rel3" `isSubstrOf` str),
                    ("4. Rel4" `isSubstrOf` str),
                    ("5. Rel5" `isSubstrOf` str),
                    ("6. Rel1" `isSubstrOf` str),
                    -- Should be a single line.
                    ((length (lines str)) == 7)])

test13 = (HandleTest "Cyclic_Proof_Bad"
                     "Ensures that cyclic proofs are detected."
                     (doTest goodGens goodRels cyclicReasoningBad)
                     check13)

test14 = (HandleTest "Cyclic_Proof_Fix"
                     "Ensures that a cyclic proof can be fixed by eliminating one link."
                     (doTest goodGens goodRels cyclicReasoningFix)
                     (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Tests semantic validation.

semGens :: FilePath
semGens = "data/test/dyadics/dyadic2.good.gens"

goodSemRels :: FilePath
goodSemRels = "data/test/dyadics/dyadic.good.rels"

badSemRels :: FilePath
badSemRels = "data/test/dyadics/dyadic.bad.rels"

missingSemRels :: FilePath
missingSemRels = "data/test/dyadics/dyadic.missing.rels"

semProof :: FilePath
semProof = "data/test/dyadics/dyadic.derivs"

checkBadSem :: String -> Bool
checkBadSem str = (and [-- Correct error message was displayed.
                       ("contradicts semantics" `isSubstrOf` str),
                       -- Correct relation was displayed.
                       ("rx" `isSubstrOf` str),
                       -- Should be a single line.
                       ((length (lines str)) == 1)])

checkMissingSem :: String -> Bool
checkMissingSem str = (and [-- Correct error message was displayed.
                            ("contains an unknown generator:" `isSubstrOf` str),
                            -- Correct relation was displayed.
                            ("rx" `isSubstrOf` str),
                            -- Should be a single line.
                            ((length (lines str)) == 1)])

test15 = (HandleTest "RelSem_Valid"
                     "Ensures that derivations can pass when the semantics are valid."
                     (doTest semGens goodSemRels [semProof])
                     (\str -> str == "Success.\n"))

test16 = (HandleTest "RelSem_Invalid"
                     "Ensures that derivations fail when semantic checks fail (1/2)."
                     (doTest semGens badSemRels [semProof])
                     checkBadSem)

test17 = (HandleTest "RelSem_Missing"
                     "Ensures that derivations fail when semantic checks fail (2/2)."
                     (doTest semGens missingSemRels [semProof])
                     checkMissingSem)

-----------------------------------------------------------------------------------------
-- Tests that a file can contain multiple proofs.

manyInFileGood :: String
manyInFileGood = "data/test/many.good.derivs"

test18 = (HandleTest "Many_Good"
                     "Ensures that a derivation file can contain multiple proofs."
                     (doTest goodGens goodRels [manyInFileGood])
                     (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Tests that derivation errors can be pinpointed in a multi-derivation file.

manyInFileBad :: String
manyInFileBad = "data/test/many.bad.derivs"

check19 :: String -> Bool
check19 str =
    if ((length outputLines) == 2)
    then (and [-- The error and its location.
               (isFailure (outputLines !! 0)),
               -- The error details.
               ("derivation(3)" `isSubstrOf` (outputLines !! 0))])
    else False
    where outputLines = lines str

test19 = (HandleTest "Many_Bad"
                     "Ensures that errors are located properly within multiple proofs."
                     (doTest goodGens goodRels [manyInFileBad])
                     check19)

-----------------------------------------------------------------------------------------
-- Tests equational derived rules.

eqnDerivedProofs :: String
eqnDerivedProofs = "data/test/equational.good.derivs"

test20 = (HandleTest "Equational_Derived_Rules"
                     "Ensures that derived rules can be applied equationally (if valid)."
                     (doTest goodGens goodRels [eqnDerivedProofs])
                     (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Tests the detection of invalid equational derived rules.

neqDerivedProofs :: String
neqDerivedProofs = "data/test/equational.bad.derivs"

check21 :: String -> Bool
check21 str = (and [-- Correct error message was displayed.
                    ("not equational" `isSubstrOf` str),
                    -- Correct derivation was displayed.
                    ("derivation(4)" `isSubstrOf` str),
                    -- Correct step was displayed.
                    ("step 1" `isSubstrOf` str),
                    -- Should be a single line.
                    ((length (lines str)) == 3)])

test21 = (HandleTest "Equational_Derived_Failure"
                     "Ensures that directed derived rules are not applied equationally."
                     (doTest goodGens goodRels [neqDerivedProofs])
                     check21)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = [test1,
         test2,
         test3,
         test4,
         test5,
         test6,
         test7,
         test8,
         test9,
         test10,
         test11,
         test12,
         test13,
         test14,
         test15,
         test16,
         test17,
         test18,
         test19,
         test20,
         test21]

main = handleTestToMain $ runAllHandleTests tests
