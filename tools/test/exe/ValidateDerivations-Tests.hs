module Main where

import Data.List
import LafontExeTest.HandleTest
import LafontExe.ValidateDerivations
import Lafont.String
import Lafont.Rewrite.Derivations

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
-- Helper methods.

isFailure :: String -> Bool
isFailure str = or [("Failed" `isSubstrOf` str),
                    ("failed" `isSubstrOf` str)]

isDuplicated :: String -> Bool
isDuplicated str = or [("Duplicated" `isSubstrOf` str),
                       ("duplicated" `isSubstrOf` str)]

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
                    (\x -> validateDerivations x goodGens goodRels [goodProofOne])
                    (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Good input, multiple derivation.

multipleGoodProofs :: [FilePath]
multipleGoodProofs = [goodProofOne, goodProofTwo]

test2 = (HandleTest "Multiple_Good_Derivation"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (\x -> validateDerivations x goodGens goodRels multipleGoodProofs)
                    (\str -> str == "Success.\n"))

-----------------------------------------------------------------------------------------
-- Bad generators.

badGens :: FilePath
badGens = "data/test/bad.gens"

check3 :: String -> Bool
check3 str = (and [-- The line at which the error should occur (see bad.gens).
                   ("data/test/bad.gens:5" `isSubstrOf` str),
                   -- The expected error.
                   ("invalid symbol" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test3 = (HandleTest "Bad_Generators"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (\x -> validateDerivations x badGens goodRels [goodProofOne])
                    check3)

-----------------------------------------------------------------------------------------
-- Bad relations.

badRels :: FilePath
badRels = "data/test/bad.rels"

check4 :: String -> Bool
check4 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.rels:4" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("xyz3" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test4 = (HandleTest "Bad_Relations"
                    "Ensures that validateDerivations accepts a valid derivation."
                    (\x -> validateDerivations x goodGens badRels [goodProofOne])
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
                    (\x -> validateDerivations x goodGens goodRels [badProofOne])
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
                    (\x -> validateDerivations x goodGens goodRels [badProofTwo])
                    check6)

-----------------------------------------------------------------------------------------
-- Bad derivation input.

badProofThree :: FilePath
badProofThree = "data/test/bad.3.derivs"

check7 :: String -> Bool
check7 str = (and [-- The line at which the error should occur (see bad.rels).
                   ("data/test/bad.3.derivs:6" `isSubstrOf` str),
                   -- The unexpected generator.
                   ("abc5" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test7 = (HandleTest "Bad_Derivation_Format"
                    "Ensures that validateDerivations rejects unexpected results."
                    (\x -> validateDerivations x goodGens goodRels [badProofThree])
                    check7)

-----------------------------------------------------------------------------------------
-- Bad derivation in list.

mixedProofs :: [FilePath]
mixedProofs = [goodProofOne, badProofOne, goodProofTwo]

test8 = (HandleTest "Bad_Derivation_Mixed"
                    "Ensures that validateDerivations detects bad derivation in a list."
                    (\x -> validateDerivations x goodGens goodRels mixedProofs)
                    check5)

-----------------------------------------------------------------------------------------
-- Tests support for derived rules.

derivedRuleProof :: String
derivedRuleProof = "data/test/summary.derivs"

derivedProofDeps = [goodProofOne, derivedRuleProof, goodProofTwo]

check8 :: String -> Bool
check8 str = (and [-- The line at which the error should occur (see summary.derivs).
                   ("data/test/summary.derivs:9" `isSubstrOf` str),
                   -- The missing relation name.
                   ("DerivedRel1" `isSubstrOf` str),
                   -- Should be a single line.
                   ((length (lines str)) == 1)])

test9 = (HandleTest "Derived_Rules_Pass"
                    "Ensures that validateDerivations identifies derived rules (1/2)."
                    (\x -> validateDerivations x goodGens goodRels [derivedRuleProof])
                    check8)

test10 = (HandleTest "Derived_Rules_Fail"
                     "Ensures that validateDerivations identifies derived rules (2/2)."
                     (\x -> validateDerivations x goodGens goodRels derivedProofDeps)
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
                    -- Should be a single line.
                    ((length (lines str)) == 1)])

test11 = (HandleTest "Duplicate_Rel_Name"
                     "Ensures that validateDerivations identifies name collisions."
                     (\x -> validateDerivations x goodGens goodRels twoProofsOneName)
                     check11)

-----------------------------------------------------------------------------------------
-- Tests that a trivial, unammed proof passes all of the checks.

test12 = (HandleTest "Trivial_Unnamed"
                     "Ensures that validateDerivations handles trivial unammed proofs."
                     (\x -> validateDerivations x goodGens goodRels [trivialProof])
                     (\str -> str == "Success.\n"))

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
         test12]

main = handleTestToMain $ runAllHandleTests tests
