module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Either
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.DerivationFile

-----------------------------------------------------------------------------------------
-- parseRewritePos

word1 :: MonWord
word1 = [(Symbol "a" []), (Symbol "b" []), (Symbol "c" [])]

word2 :: MonWord
word2 = [(Symbol "c" []), (Symbol "a" []), (Symbol "b" [])]

primitiveRuleL2R :: RewriteRule
primitiveRuleL2R = RewriteRule word1 word2 False False

primitiveRuleEqn :: RewriteRule
primitiveRuleEqn = RewriteRule word1 word2 True False

test1 = TestCase (assertEqual "parseRewritePos rejects empty strings (1/2)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R False ""))

test2 = TestCase (assertEqual "parseRewritePos rejects empty strings (2/2)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R True ""))

test3 = TestCase (assertEqual "parseRewritePos rejects bad positions (1/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R False "asd"))

test4 = TestCase (assertEqual "parseRewritePos rejects bad positions (2/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R True "asd"))

test5 = TestCase (assertEqual "parseRewritePos rejects bad positions (3/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R False "-4"))

test6 = TestCase (assertEqual "parseRewritePos rejects bad positions (4/4)."
                              (Left (Right InvalidRewritePos))
                              (parseRewritePos primitiveRuleL2R True "-4"))

test7 = TestCase (assertEqual "parseRewritePos accepts natural numbers (1/5)."
                              (Right (Rewrite primitiveRuleL2R 10 True))
                              (parseRewritePos primitiveRuleL2R True "10"))

test8 = TestCase (assertEqual "parseRewritePos accepts natural numbers (2/5)."
                              (Right (Rewrite primitiveRuleL2R 10 False))
                              (parseRewritePos primitiveRuleL2R False "10"))

test9 = TestCase (assertEqual "parseRewritePos accepts natural numbers (3/5)."
                              (Right (Rewrite primitiveRuleEqn 10 True))
                              (parseRewritePos primitiveRuleEqn True "10"))

test10 = TestCase (assertEqual "parseRewritePos accepts natural numbers (4/5)."
                               (Right (Rewrite primitiveRuleEqn 10 False))
                               (parseRewritePos primitiveRuleEqn False "10"))

test11 = TestCase (assertEqual "parseRewritePos accepts natural numbers (5/5)."
                               (Right (Rewrite primitiveRuleL2R 5 True))
                               (parseRewritePos primitiveRuleL2R True "5"))

test12 = TestCase (assertEqual "parseRewritePos handles trailing spacing."
                               (Right (Rewrite primitiveRuleL2R 1234 True))
                               (parseRewritePos primitiveRuleL2R True "1234  \t\t   "))

test13 = TestCase (assertEqual "parseRewritePos rejects symbols after trailing spacing."
                               (Left (Left (UnexpectedSymbol 4)))
                               (parseRewritePos primitiveRuleL2R True "1234  \t\t   xyz"))

-----------------------------------------------------------------------------------------
-- parseRewriteDirAndPos

test14 = TestCase (assertEqual "parseRewriteDirAndPos propgates errors correctly."
                               (Left (Right InvalidRewritePos))
                               (parseRewriteDirAndPos primitiveRuleL2R "" "asd"))

test15 = TestCase (assertEqual "parseRewriteDirAndPos direction misalignment (1/2)."
                               (Left (Right InvalidRewriteDir))
                               (parseRewriteDirAndPos primitiveRuleL2R "←" "10"))

test16 = TestCase (assertEqual "parseRewriteDirAndPos direction misalignment (2/2)."
                               (Left (Right MissingRewriteDir))
                               (parseRewriteDirAndPos primitiveRuleEqn "" "10"))

test17 = TestCase (assertEqual "parseRewriteDirAndPos accepts equational rules with →."
                               (Right (Rewrite primitiveRuleEqn 10 True))
                               (parseRewriteDirAndPos primitiveRuleEqn "→" "10  \t"))

test18 = TestCase (assertEqual "parseRewriteDirAndPos accepts equational rules with ←."
                               (Right (Rewrite primitiveRuleEqn 10 False))
                               (parseRewriteDirAndPos primitiveRuleEqn "←" "10  \t"))

test19 = TestCase (assertEqual "parseRewriteDirAndPos accepts production rules without dirs."
                               (Right (Rewrite primitiveRuleL2R 0 True))
                               (parseRewriteDirAndPos primitiveRuleL2R "" "0  \t"))

test20 = TestCase (assertEqual "parseRewriteDirAndPos accepts production rules with →."
                               (Right (Rewrite primitiveRuleL2R 0 True))
                               (parseRewriteDirAndPos primitiveRuleL2R "→" "0  \t"))

-----------------------------------------------------------------------------------------
-- parseRewrite

derivedRule :: RewriteRule
derivedRule = RewriteRule word1 word2 False True

dict0 :: RuleDict
dict0 = empty
dict1 = addRule dict0 ("abc", primitiveRuleL2R)
dict2 = addRule dict1 ("xyz", primitiveRuleEqn)
dict3 = addRule dict2 ("derived", derivedRule)

test21 = TestCase (assertEqual "parseRewrite handles empty strings."
                               (Left (Right InvalidRuleName))
                               (parseRewrite dict3 ""))

test22 = TestCase (assertEqual "parseRewrite rejects bad rules identifiers."
                               (Left (Right InvalidRuleName))
                               (parseRewrite dict3 "1abc"))

test23 = TestCase (assertEqual "parseRewrite rejects invalid rules identifiers."
                               (Left (Right (UnknownRuleName "bad")))
                               (parseRewrite dict3 "bad"))

test24 = TestCase (assertEqual "parseRewrite requires at least a position."
                               (Left (Right InvalidRewritePos))
                               (parseRewrite dict3 "abc"))

test25 = TestCase (assertEqual "parseRewrite propogates errors correctly."
                               (Left (Left (UnexpectedSymbol 6)))
                               (parseRewrite dict3 "abc 10 x"))

test26 = TestCase (assertEqual "parseRewrite supports rewrites without directions."
                               (Right (Rewrite primitiveRuleL2R 0 True))
                               (parseRewrite dict3 "abc  0   "))

test27 = TestCase (assertEqual "parseRewrite supports rewrites with direction →."
                               (Right (Rewrite primitiveRuleEqn 0 True))
                               (parseRewrite dict3 "xyz  →   0   "))

test28 = TestCase (assertEqual "parseRewrite supports rewrites with direction ←."
                               (Right (Rewrite primitiveRuleEqn 0 False))
                               (parseRewrite dict3 "xyz  ←   0   "))

test29 = TestCase (assertEqual "parseRewrite can support different rewrite positions."
                               (Right (Rewrite primitiveRuleL2R 1 True))
                               (parseRewrite dict3 "abc 1"))

-----------------------------------------------------------------------------------------
-- parseRewriteLine

test30 = TestCase (assertEqual ""
                               (Left (Right InvalidRuleName))
                               (parseRewriteLine dict3 ""))

test31 = TestCase (assertEqual ""
                               (Left (Right UnknownRewriteMod))
                               (parseRewriteLine dict3 "!badmod"))

test32 = TestCase (assertEqual ""
                               (Left (Left (UnexpectedSymbol 6)))
                               (parseRewriteLine dict3 "abc 10 x"))

test33 = TestCase (assertEqual ""
                               (Left (Left (UnexpectedSymbol 19)))
                               (parseRewriteLine dict3 "!apply   derived 10 x"))

test34 = TestCase (assertEqual ""
                               (Left (Right RewriteOnDerived))
                               (parseRewriteLine dict3 "derived 10"))

test35 = TestCase (assertEqual ""
                               (Left (Right ApplyOnPrimitive))
                               (parseRewriteLine dict3 "!apply abc 10"))

test36 = TestCase (assertEqual ""
                               (Right (Rewrite primitiveRuleEqn 0 False))
                               (parseRewriteLine dict3 "xyz  ←   0   "))

test37 = TestCase (assertEqual ""
                               (Right (Rewrite derivedRule 10 True))
                               (parseRewriteLine dict3 "!apply  derived   →    10     "))

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
goodBody = ["  x1.x2.x3",
            "",
            "abc 0",
            "xyz → 5",
            "!apply derived 10",
            "",
            "",
            "  x3.x2.x1",
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
                                     TestLabel "parseRewritePreamble_Unknown" test40]

main = defaultMain tests
