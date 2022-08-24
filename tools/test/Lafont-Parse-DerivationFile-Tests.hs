module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Either
import Lafont.Common
import Lafont.Rewrite.Abstraction
import Lafont.Rewrite.Common
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Lookup
import Lafont.Rewrite.Summary
import Lafont.Parse.Common
import Lafont.Parse.DerivationFile
import Lafont.Parse.Internal.DerivationFile

-----------------------------------------------------------------------------------------
-- parseRewriteAtPos

test1 = TestCase (assertEqual "parseRewriteAtPos rejects empty strings (1/2)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos False ""))

test2 = TestCase (assertEqual "parseRewriteAtPos rejects empty strings (2/2)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos True ""))

test3 = TestCase (assertEqual "parseRewriteAtPos rejects bad positions (1/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos False "asd"))

test4 = TestCase (assertEqual "parseRewriteAtPos rejects bad positions (2/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos True "asd"))

test5 = TestCase (assertEqual "parseRewriteAtPos rejects bad positions (3/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos False "-4"))

test6 = TestCase (assertEqual "parseRewriteAtPos rejects bad positions (4/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewriteAtPos True "-4"))

test7 = TestCase (assertEqual "parseRewriteAtPos accepts natural numbers (1/5)."
                              (Right (10, L2R))
                              (parseRewriteAtPos True "10"))

test8 = TestCase (assertEqual "parseRewriteAtPos accepts natural numbers (2/5)."
                              (Right (10, R2L))
                              (parseRewriteAtPos False "10"))

test9 = TestCase (assertEqual "parseRewriteAtPos accepts natural numbers (3/5)."
                              (Right (10, L2R))
                              (parseRewriteAtPos True "10"))

test10 = TestCase (assertEqual "parseRewriteAtPos accepts natural numbers (4/5)."
                               (Right (10, R2L))
                               (parseRewriteAtPos False "10"))

test11 = TestCase (assertEqual "parseRewriteAtPos accepts natural numbers (5/5)."
                               (Right (5, L2R))
                               (parseRewriteAtPos True "5"))

test12 = TestCase (assertEqual "parseRewriteAtPos handles trailing spacing."
                               (Right (1234, L2R))
                               (parseRewriteAtPos True "1234  \t\t   "))

test13 = TestCase (assertEqual "parseRewriteAtPos rejects symbols after trailing spacing."
                               (Left (Left (UnexpectedSymbol 4)))
                               (parseRewriteAtPos True "1234  \t\t   xyz"))

-----------------------------------------------------------------------------------------
-- parseRewriteAtDirAndPos

test14 = TestCase (assertEqual "parseRewriteAtDirAndPos propgates errors correctly."
                               (Left (Right InvalidRewritePos))
                               (parseRewriteAtDirAndPos False "" "asd"))

test15 = TestCase (assertEqual "parseRewriteAtDirAndPos direction misalignment (1/2)."
                               (Left (Right InvalidRewriteDir))
                               (parseRewriteAtDirAndPos False "←" "10"))

test16 = TestCase (assertEqual "parseRewriteAtDirAndPos direction misalignment (2/2)."
                               (Left (Right MissingRewriteDir))
                               (parseRewriteAtDirAndPos True "" "10"))

test17 = TestCase (assertEqual "parseRewriteAtDirAndPos accepts equational rules with →."
                               (Right (10, L2R))
                               (parseRewriteAtDirAndPos True "→" "10  \t"))

test18 = TestCase (assertEqual "parseRewriteAtDirAndPos accepts equational rules with ←."
                               (Right (10, R2L))
                               (parseRewriteAtDirAndPos True "←" "10  \t"))

test19 = TestCase (assertEqual "parseRewriteAtDirAndPos accepts production rules without dirs."
                               (Right (0, L2R))
                               (parseRewriteAtDirAndPos False "" "0  \t"))

test20 = TestCase (assertEqual "parseRewriteAtDirAndPos accepts production rules with →."
                               (Right (0, L2R))
                               (parseRewriteAtDirAndPos False "→" "0  \t"))

-----------------------------------------------------------------------------------------
-- parseRewrite

word1 :: MonWord
word1 = [(Symbol "a" []), (Symbol "b" []), (Symbol "c" [])]

word2 :: MonWord
word2 = [(Symbol "c" []), (Symbol "a" []), (Symbol "b" [])]

primitiveRuleL2R :: RewriteRule
primitiveRuleL2R = RewriteRule word1 word2 False Nothing

primitiveRuleEqn :: RewriteRule
primitiveRuleEqn = RewriteRule word1 word2 True Nothing

derivedRule :: RewriteRule
derivedRule = RewriteRule word1 word2 False (Just "derived")

dict0 :: RuleDict
dict0 = empty
dict1 = addRule dict0 ("abc", primitiveRuleL2R)
dict2 = addRule dict1 ("xyz", primitiveRuleEqn)
-- dict3 = addRule dict2 ("derived", derivedRule)

test21 = TestCase (assertEqual "parseRewrite handles empty strings."
                               (Left (Right InvalidRuleName))
                               (parseRewrite dict2 ""))

test22 = TestCase (assertEqual "parseRewrite rejects bad rules identifiers."
                               (Left (Right InvalidRuleName))
                               (parseRewrite dict2 "1abc"))

test23 = TestCase (assertEqual "parseRewrite rejects invalid rules identifiers."
                               (Left (Right (UnknownRuleName "bad")))
                               (parseRewrite dict2 "bad"))

test24 = TestCase (assertEqual "parseRewrite requires at least a position."
                               (Left (Right InvalidRewritePos))
                               (parseRewrite dict2 "abc"))

test25 = TestCase (assertEqual "parseRewrite propogates errors correctly."
                               (Left (Left (UnexpectedSymbol 6)))
                               (parseRewrite dict2 "abc 10 x"))

test26 = TestCase (assertEqual "parseRewrite supports rewrites without directions."
                               (Right (Rewrite primitiveRuleL2R 0 L2R))
                               (parseRewrite dict2 "abc  0   "))

test27 = TestCase (assertEqual "parseRewrite supports rewrites with direction →."
                               (Right (Rewrite primitiveRuleEqn 0 L2R))
                               (parseRewrite dict2 "xyz  →   0   "))

test28 = TestCase (assertEqual "parseRewrite supports rewrites with direction ←."
                               (Right (Rewrite primitiveRuleEqn 0 R2L))
                               (parseRewrite dict2 "xyz  ←   0   "))

test29 = TestCase (assertEqual "parseRewrite can support different rewrite positions."
                               (Right (Rewrite primitiveRuleL2R 1 L2R))
                               (parseRewrite dict2 "abc 1"))

-----------------------------------------------------------------------------------------
-- parseRewriteLine

drel1 = nullRuleSet `addDerivedRule` "derived"

test30 = TestCase (assertEqual "parseRewriteLine rejects invalid relation names."
                               (Left (Right InvalidRuleName))
                               (parseRewriteLine dict2 drel1 ""))

test31 = TestCase (assertEqual "parseRewriteLine rejects invalid modifiers."
                               (Left (Right UnknownRewriteMod))
                               (parseRewriteLine dict2 drel1 "!badmod"))

test32 = TestCase (assertEqual "parseRewriteLine identifies trailing symbols in rewrite."
                               (Left (Left (UnexpectedSymbol 6)))
                               (parseRewriteLine dict2 drel1 "abc 10 x"))

test33 = TestCase (assertEqual "parseRewriteLine identifies trailing symbols in apply."
                               (Left (Left (UnexpectedSymbol 20)))
                               (parseRewriteLine dict2 drel1 "!apply   derived → 10 x"))

test34 = TestCase (assertEqual "parseRewriteLine ensures primitive rules exist."
                               (Left (Right (UnknownRuleName "derived")))
                               (parseRewriteLine dict2 drel1 "derived 10"))

test35 = TestCase (assertEqual "parseRewriteLine ensures derived rules exist."
                               (Left (Right (UnknownDerivedRule "abc")))
                               (parseRewriteLine dict2 drel1 "!apply abc 10"))

test36 = TestCase (assertEqual "parseRewriteLine can parse rewrites."
                               (Right (Left (Rewrite primitiveRuleEqn 0 R2L)))
                               (parseRewriteLine dict2 drel1 "xyz  ←   0   "))

test37 = TestCase (assertEqual "parseRewriteLine can parse applications."
                               (Right (Right (Apply "derived" 10 L2R)))
                               (parseRewriteLine dict2 drel1 "!apply  derived   →    10     "))

-----------------------------------------------------------------------------------------
-- Preamble parsing.

goodPreamble :: [String]
goodPreamble = ["-- Preamble",
                "\t\t-- TODO: Update name.",
                "@name tmp -- Placeholder",
                "",
                "-- Start of proof.",
                ""]

goodBody :: [String]
goodBody = ["  a.b.c",
            "",
            "abc 0",
            "xyz → 5",
            "!apply derived → 10",
            "",
            ""]

expectedPreamble :: RewritePreamble
expectedPreamble = RewritePreamble (Just "tmp")

badPreambleOne :: [String]
badPreambleOne = ["@name tmp", "@name tmp"]

badPreambleTwo :: [String]
badPreambleTwo = ["@name tmp", "@unknown tmp"]

test38 = TestCase (assertEqual "Rewrite preamble parser works on valid files."
                               (Right (goodBody, 6, expectedPreamble))
                               (parseRewritePreamble (goodPreamble ++ goodBody) 0))

test39 = TestCase (assertEqual "Rewrite preamble parser works on valid files."
                               (Left (1, DuplicateProp "name"))
                               (parseRewritePreamble badPreambleOne 0))

test40 = TestCase (assertEqual "Rewrite preamble parser works on valid files."
                               (Left (1, UnknownProp "unknown"))
                               (parseRewritePreamble badPreambleTwo 0))

-----------------------------------------------------------------------------------------
-- parseFinalMonWord

altBody :: [String]
altBody = ["",
           "abc 0",
           "xyz → 5",
           "!apply derived → 10",
           "",
           ""]

goodEOF :: [String]
goodEOF = ["\t\t", "-- END OF FILE."]

goodFinal1 :: [String]
goodFinal1 = ["  a.b.c  "] ++ goodEOF

goodFinal2 :: [String]
goodFinal2 = ["  c.a.b  "] ++ goodEOF

goodRewrite :: [String]
goodRewrite = tail goodBody

test41 = TestCase (assertEqual "parseFinalMonWord supports empty files."
                               Nothing
                               (parseFinalMonWord []))

test42 = TestCase (assertEqual "parseFinalMonWord extracts final words (1/3)."
                               (Just (altBody, word1, goodEOF))
                               (parseFinalMonWord (altBody ++ goodFinal1)))

test43 = TestCase (assertEqual "parseFinalMonWord extracts final words (2/3)."
                               (Just (altBody, word2, goodEOF))
                               (parseFinalMonWord (altBody ++ goodFinal2)))

test44 = TestCase (assertEqual "parseFinalMonWord extracts final words (3/3)."
                               (Just (altBody, word2, goodEOF ++ goodFinal1))
                               (parseFinalMonWord (altBody ++ goodFinal2 ++ goodFinal1)))

test45 = TestCase (assertEqual "parseFinalMonWord without final mon word works."
                               Nothing
                               (parseFinalMonWord goodRewrite))

-----------------------------------------------------------------------------------------
-- parseRewriteLines

-- Single line tests.

test46 = TestCase (assertEqual "parseRewriteLines supports empty files."
                               (Right [])
                               (parseRewriteLines dict2 drel1 [] 0))

test47 = TestCase (assertEqual "parseRewriteLines can parse a single line."
                               (Right [Left (Rewrite primitiveRuleEqn 5 R2L)])
                               (parseRewriteLines dict2 drel1 ["  xyz  ←   5   -- xyz"] 0))

test48 = TestCase (assertEqual "parseRewriteLines can propogate errors (single line)."
                               (Left (0, Left (UnexpectedSymbol 20)))
                               (parseRewriteLines dict2 drel1 ["  !apply derived ← 10 x --x"] 0))

-- Multi-line tests.

rewriteList :: [AbsRewrite]
rewriteList = [Left (Rewrite primitiveRuleL2R 0 L2R),
               Left (Rewrite primitiveRuleEqn 5 L2R),
               Right (Apply "derived" 10 L2R)]

test49 = TestCase (assertEqual "parseRewriteLines can parse multiple valid lines."
                               (Right rewriteList)
                               (parseRewriteLines dict2 drel1 goodRewrite 0))

test50 = TestCase (assertEqual "parseRewriteLines can propogate errors (multiline)."
                               (Left (6, Left (UnexpectedSymbol 6)))
                               (parseRewriteLines dict2 drel1 (goodRewrite ++ [" abc 0 x"]) 0))

-- Adjusted starting line.

test51 = TestCase (assertEqual "parseRewriteLines supports offsets (1/2)."
                               (Right rewriteList)
                               (parseRewriteLines dict2 drel1 goodRewrite 5))

test52 = TestCase (assertEqual "parseRewriteLines supports offsets (2/2)."
                               (Left (11, Left (UnexpectedSymbol 6)))
                               (parseRewriteLines dict2 drel1 (goodRewrite ++ [" abc 0 x"]) 5))

-----------------------------------------------------------------------------------------
-- preparseDerivationFile

gens :: [String]
gens = ["a", "b", "c"]

badBody :: [String]
badBody = ["a.b.c",
           "",
           "abc 0",
           "1abc 0",
           "",
           "",
           "a.b.c"]

parseBody :: [String] -> RewritePreamble -> [String] -> DParseRV AbsDerivation
parseBody gens meta lines =
    case preparseSectionSkeleton lines 0 of
        Left err               -> Left err
        Right (skeleton, rest) -> case preparseSection gens meta skeleton 0 of
            Left err  -> Left err
            Right pre -> parseDerivationFile dict2 drel1 pre

test53 = TestCase (assertEqual "preparseBody parses a full derivation (1/3)."
                               (Right (AbsDerivation sum rewriteList))
                               (parseBody gens defaultPreamble input))
    where input = goodBody ++ goodFinal1
          sum = DerivationSummary defaultPreamble word1 word1

test54 = TestCase (assertEqual "preparseBody parses a full derivation (2/3)."
                               (Right (AbsDerivation sum rewriteList))
                               (parseBody gens defaultPreamble input))
    where input = goodBody ++ goodFinal2
          sum = DerivationSummary defaultPreamble word1 word2

test55 = TestCase (assertEqual "preparseBody parses a full derivation (3/3)."
                               (Right (AbsDerivation sum rewriteList))
                               (parseBody gens defaultPreamble input))
    where input = goodFinal1 ++ goodRewrite ++ goodFinal1
          sum = DerivationSummary defaultPreamble word1 word1

test56 = TestCase (assertEqual "preparseBody detects missing initial word."
                               (Left (0, Right MissingInitialWord))
                               (parseBody gens defaultPreamble input))
    where input = goodRewrite ++ goodFinal1

test57 = TestCase (assertEqual "preparseBody detects delayed initial word."
                               (Left (0, Right MissingInitialWord))
                               (parseBody gens defaultPreamble input))
    where input = [""] ++ goodFinal1 ++ goodRewrite ++ goodFinal1

test58 = TestCase (assertEqual "preparseBody detects empty inputs."
                               (Left (0, Left UnexpectedEOF))
                               (parseBody gens defaultPreamble []))

test59 = TestCase (assertEqual "preparseBody detects bad generators in initial word."
                               (Left (0, Right (UnknownGenName "c")))
                               (parseBody ["a", "b"] defaultPreamble input))
    where input = goodBody ++ goodFinal1

test60 = TestCase (assertEqual "preparseBody detects bad generators in final word."
                               (Left (7, Right (UnknownGenName "c")))
                               (parseBody ["a", "b"] defaultPreamble input))
    where input = ["ε"] ++ goodRewrite ++ goodFinal1

test61 = TestCase (assertEqual "preparseBody detects missing final word."
                               (Left (7, Right MissingFinalWord))
                               (parseBody gens defaultPreamble goodBody))

test62 = TestCase (assertEqual "preparseBody detects rewrite issues."
                               (Left (3, Right InvalidRuleName))
                               (parseBody gens defaultPreamble badBody))

test63 = TestCase (assertEqual "preparseBody supports different preambles."
                               (Right (AbsDerivation sum rewriteList))
                               (parseBody gens expectedPreamble input))
    where input = goodBody ++ goodFinal1
          sum = DerivationSummary expectedPreamble word1 word1

-----------------------------------------------------------------------------------------
-- parseDerivationFile

validResult1 :: AbsDerivation
validResult1 = AbsDerivation (DerivationSummary expectedPreamble word1 word2) rewriteList

validResult2 :: AbsDerivation
validResult2 = AbsDerivation (DerivationSummary defaultPreamble word1 word1) rewriteList

parseFile :: [String] -> [String] -> Int -> DParseRV [DParseRV AbsDerivation]
parseFile gens lines num =
    case (preparseDerivationFile gens lines num) of
        Left err      -> Left err
        Right preList -> Right (map (parseDerivationFile dict2 drel1) preList)

test64 = TestCase (assertEqual "parseDerivationFile parses a full derivation file."
                               (Right [Right validResult1])
                               (parseFile gens input 0))
    where input = goodPreamble ++ goodBody ++ goodFinal2

test65 = TestCase (assertEqual "preparseDerivationFile propogates preamble errors."
                               (Left (1, Left (DuplicateProp "name")))
                               (parseFile gens input 0))
    where input = badPreambleOne ++ goodBody ++ goodFinal2

test66 = TestCase (assertEqual "preparseDerivationFile propogates body errors."
                               (Left (13, Right (UnknownGenName "c")))
                               (parseFile ["a", "b"] input 0))
    where input = goodPreamble ++ ["ε"] ++ goodRewrite ++ goodFinal1

test67 = TestCase (assertEqual "preparseDerivationFile propogates body errors."
                               (Right [Left (3, Right InvalidRuleName)])
                               (parseFile gens badBody 0))

test68 = TestCase (assertEqual "parseDerivationFile supports multiple derivations."
                               (Right [Right validResult1, Right validResult2])
                               (parseFile gens input 0))
    where input = goodPreamble ++ goodBody ++ goodFinal2 ++ goodBody ++ goodFinal1

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseRewritePos_EmptyStringOne" test1,
                                     TestLabel "parseRewritePos_EmptyStringTwo" test2,
                                     TestLabel "parseRewritePos_BadPosOne" test3,
                                     TestLabel "parseRewritePos_BadPosTwo" test4,
                                     TestLabel "parseRewritePos_BadPosThree" test5,
                                     TestLabel "parseRewritePos_BadPosFour" test6,
                                     TestLabel "parseRewritePos_GoodPosOne" test7,
                                     TestLabel "parseRewritePos_GoodPosTwo" test8,
                                     TestLabel "parseRewritePos_GoodPosThree" test9,
                                     TestLabel "parseRewritePos_GoodPosFour" test10,
                                     TestLabel "parseRewritePos_GoodPosFive" test11,
                                     TestLabel "parseRewritePos_TrailingSpacing" test12,
                                     TestLabel "parseRewritePos_TrailingSymbols" test13,
                                     TestLabel "parseRewriteDirAndPos_Propogates" test14,
                                     TestLabel "parseRewriteDirAndPos_MisalignedOne" test15,
                                     TestLabel "parseRewriteDirAndPos_MisalignedTwo" test16,
                                     TestLabel "parseRewriteDirAndPos_Eqn_L2R" test17,
                                     TestLabel "parseRewriteDirAndPos_Eqn_R2L" test18,
                                     TestLabel "parseRewriteDirAndPos_L2R_Inferred" test19,
                                     TestLabel "parseRewriteDirAndPos_L2R_Explicit" test20,
                                     TestLabel "parseRewrite_EmptyString" test21,
                                     TestLabel "parseRewrite_BadRuleID" test22,
                                     TestLabel "parseRewrite_UnknownRule" test23,
                                     TestLabel "parseRewrite_MissingPos" test24,
                                     TestLabel "parseRewrite_ErrorProp" test25,
                                     TestLabel "parseRewrite_NoDir" test26,
                                     TestLabel "parseRewrite_L2R" test27,
                                     TestLabel "parseRewrite_R2L" test28,
                                     TestLabel "parseRewrite_OtherPos" test29,
                                     TestLabel "parseRewriteLine_EmptyString" test30,
                                     TestLabel "parseRewriteLine_BadMod" test31,
                                     TestLabel "parseRewriteLine_ErrorProp_NoApply" test32,
                                     TestLabel "parseRewriteLine_ErrorProp_Apply" test33,
                                     TestLabel "parseRewriteLine_DerivedAsPrimitive" test34,
                                     TestLabel "parseRewriteLine_PrimitiveAsDerived" test35,
                                     TestLabel "parseRewriteLine_PrimitiveParse" test36,
                                     TestLabel "parseRewriteLine_DerivedParse" test37,
                                     TestLabel "parseRewritePreamble_Valid" test38,
                                     TestLabel "parseRewritePreamble_Duplicate" test39,
                                     TestLabel "parseRewritePreamble_Unknown" test40,
                                     TestLabel "parseFinalMonWord_EmptyInput" test41,
                                     TestLabel "parseFinalMonWord_ValidOne" test42,
                                     TestLabel "parseFinalMonWord_ValidTwo" test43,
                                     TestLabel "parseFinalMonWord_ValidThree" test44,
                                     TestLabel "parseFinalMonWord_MissingFinalWord" test45,
                                     TestLabel "parseRewriteLines_EmptyFile" test46,
                                     TestLabel "parseRewriteLines_SingleValid" test47,
                                     TestLabel "parseRewriteLines_SinglePropogate" test48,
                                     TestLabel "parseRewriteLines_MultilineValid" test49,
                                     TestLabel "parseRewriteLines_MultilineError" test50,
                                     TestLabel "parseRewriteLines_OffsetOne" test51,
                                     TestLabel "parseRewriteLines_OffsetTwo" test52,
                                     TestLabel "preparseBody_ValidOne" test53,
                                     TestLabel "preparseBody_ValidTwo" test54,
                                     TestLabel "preparseBody_ValidThree" test55,
                                     TestLabel "preparseBody_MissingInitWord" test56,
                                     TestLabel "preparseBody_DelayedInitWord" test57,
                                     TestLabel "preparseBody_EmptyString" test58,
                                     TestLabel "preparseBody_InitWordBadGen" test59,
                                     TestLabel "preparseBody_FinalWordBadGen" test60,
                                     TestLabel "preparseBody_MissingFinalWord" test61,
                                     TestLabel "preparseBody_InvalidRewrite" test62,
                                     TestLabel "preparseBody_DifferentPreamble" test63,
                                     TestLabel "parseDerivationFile_Valid" test64,
                                     TestLabel "parseDerivationFile_BadPreamble" test65,
                                     TestLabel "parseDerivationFile_BadWord" test66,
                                     TestLabel "parseDerivationFile_BadBody" test67,
                                     TestLabel "parseDerivationFile_Many" test68]

main = defaultMain tests
