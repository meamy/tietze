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
-- parseAppPos

circ1 :: Circuit
circ1 = [(Gate "a" []), (Gate "b" []), (Gate "c" [])]

circ2 :: Circuit
circ2 = [(Gate "c" []), (Gate "a" []), (Gate "b" [])]

primitiveRelL2R :: RewriteRule
primitiveRelL2R = RewriteRule circ1 circ2 False False

primitiveRelEqn :: RewriteRule
primitiveRelEqn = RewriteRule circ1 circ2 True False

test1 = TestCase (assertEqual "parseAppPos rejects empty strings (1/2)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R False ""))

test2 = TestCase (assertEqual "parseAppPos rejects empty strings (2/2)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R True ""))

test3 = TestCase (assertEqual "parseAppPos rejects bad positions (1/4)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R False "asd"))

test4 = TestCase (assertEqual "parseAppPos rejects bad positions (2/4)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R True "asd"))

test5 = TestCase (assertEqual "parseAppPos rejects bad positions (3/4)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R False "-4"))

test6 = TestCase (assertEqual "parseAppPos rejects bad positions (4/4)."
                              (Left (Right InvalidAppPos))
                              (parseAppPos primitiveRelL2R True "-4"))

test7 = TestCase (assertEqual "parseAppPos accepts natural numbers (1/5)."
                              (Right (RewriteOp primitiveRelL2R 10 True))
                              (parseAppPos primitiveRelL2R True "10"))

test8 = TestCase (assertEqual "parseAppPos accepts natural numbers (2/5)."
                              (Right (RewriteOp primitiveRelL2R 10 False))
                              (parseAppPos primitiveRelL2R False "10"))

test9 = TestCase (assertEqual "parseAppPos accepts natural numbers (3/5)."
                              (Right (RewriteOp primitiveRelEqn 10 True))
                              (parseAppPos primitiveRelEqn True "10"))

test10 = TestCase (assertEqual "parseAppPos accepts natural numbers (4/5)."
                               (Right (RewriteOp primitiveRelEqn 10 False))
                               (parseAppPos primitiveRelEqn False "10"))

test11 = TestCase (assertEqual "parseAppPos accepts natural numbers (5/5)."
                               (Right (RewriteOp primitiveRelL2R 5 True))
                               (parseAppPos primitiveRelL2R True "5"))

test12 = TestCase (assertEqual "parseAppPos handles trailing spacing."
                               (Right (RewriteOp primitiveRelL2R 1234 True))
                               (parseAppPos primitiveRelL2R True "1234  \t\t   "))

test13 = TestCase (assertEqual "parseAppPos rejects symbols after trailing spacing."
                               (Left (Left (UnexpectedSymbol 4)))
                               (parseAppPos primitiveRelL2R True "1234  \t\t   xyz"))

-----------------------------------------------------------------------------------------
-- parseAppDirAndPos

test14 = TestCase (assertEqual "parseAppDirAndPos propgates errors correctly."
                               (Left (Right InvalidAppPos))
                               (parseAppDirAndPos primitiveRelL2R "" "asd"))

test15 = TestCase (assertEqual "parseAppDirAndPos direction misalignment (1/2)."
                               (Left (Right InvalidAppDir))
                               (parseAppDirAndPos primitiveRelL2R "←" "10"))

test16 = TestCase (assertEqual "parseAppDirAndPos direction misalignment (2/2)."
                               (Left (Right MissingAppDir))
                               (parseAppDirAndPos primitiveRelEqn "" "10"))

test17 = TestCase (assertEqual "parseAppDirAndPos accepts equational relations with →."
                               (Right (RewriteOp primitiveRelEqn 10 True))
                               (parseAppDirAndPos primitiveRelEqn "→" "10  \t"))

test18 = TestCase (assertEqual "parseAppDirAndPos accepts equational relations with ←."
                               (Right (RewriteOp primitiveRelEqn 10 False))
                               (parseAppDirAndPos primitiveRelEqn "←" "10  \t"))

test19 = TestCase (assertEqual "parseAppDirAndPos accepts production rules without dirs."
                               (Right (RewriteOp primitiveRelL2R 0 True))
                               (parseAppDirAndPos primitiveRelL2R "" "0  \t"))

test20 = TestCase (assertEqual "parseAppDirAndPos accepts production rules with →."
                               (Right (RewriteOp primitiveRelL2R 0 True))
                               (parseAppDirAndPos primitiveRelL2R "→" "0  \t"))

-----------------------------------------------------------------------------------------
-- parseApp

dict0 :: RelDict
dict0 = empty
dict1 = addRel dict0 ("abc", primitiveRelL2R)
dict2 = addRel dict1 ("xyz", primitiveRelEqn)

test21 = TestCase (assertEqual "parseApp handles empty strings."
                               (Left (Right InvalidRelName))
                               (parseApp dict2 ""))

test22 = TestCase (assertEqual "parseApp rejects bad relation identifiers."
                               (Left (Right InvalidRelName))
                               (parseApp dict2 "1abc"))

test23 = TestCase (assertEqual "parseApp rejects invalid relation identifiers."
                               (Left (Right (UnknownRelName "bad")))
                               (parseApp dict2 "bad"))

test24 = TestCase (assertEqual "parseApp requires at least a position."
                               (Left (Right InvalidAppPos))
                               (parseApp dict2 "abc"))

test25 = TestCase (assertEqual "parseApp propogates errors correctly."
                               (Left (Left (UnexpectedSymbol 6)))
                               (parseApp dict2 "abc 10 x"))

test26 = TestCase (assertEqual "parseApp supports operations without directions."
                               (Right (RewriteOp primitiveRelL2R 0 True))
                               (parseApp dict2 "abc  0   "))

test27 = TestCase (assertEqual "parseApp supports operations with direction →."
                               (Right (RewriteOp primitiveRelEqn 0 True))
                               (parseApp dict2 "xyz  →   0   "))

test28 = TestCase (assertEqual "parseApp supports operations with direction ←."
                               (Right (RewriteOp primitiveRelEqn 0 False))
                               (parseApp dict2 "xyz  ←   0   "))

test29 = TestCase (assertEqual "parseApp can support different application positions."
                               (Right (RewriteOp primitiveRelL2R 1 True))
                               (parseApp dict2 "abc 1"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseAppPos_EmptyStringOne" test1,
                                     TestLabel "parseAppPos_EmptyStringTwo" test2,
                                     TestLabel "parseAppPos_BadPosOne" test3,
                                     TestLabel "parseAppPos_BadPosTwo" test4,
                                     TestLabel "parseAppPos_BadPosThree" test5,
                                     TestLabel "parseAppPos_BadPosFour" test6,
                                     TestLabel "parseAppPos_GoodPosOne" test7,
                                     TestLabel "parseAppPos_GoodPosTwo" test8,
                                     TestLabel "parseAppPos_GoodPosThree" test9,
                                     TestLabel "parseAppPos_GoodPosFour" test10,
                                     TestLabel "parseAppPos_GoodPosFive" test11,
                                     TestLabel "parseAppPos_TrailingSpacing" test12,
                                     TestLabel "parseAppPos_TrailingSymbols" test13,
                                     TestLabel "parseAppDirAndPos_Propogates" test14,
                                     TestLabel "parseAppDirAndPos_MisalignedOne" test15,
                                     TestLabel "parseAppDirAndPos_MisalignedTwo" test16,
                                     TestLabel "parseAppDirAndPos_Eqn_L2R" test17,
                                     TestLabel "parseAppDirAndPos_Eqn_R2L" test18,
                                     TestLabel "parseAppDirAndPos_L2R_Inferred" test19,
                                     TestLabel "parseAppDirAndPos_L2R_Explicit" test20,
                                     TestLabel "parseApp_EmptyString" test21,
                                     TestLabel "parseApp_BadRelID" test22,
                                     TestLabel "parseApp_UnknownRel" test23,
                                     TestLabel "parseApp_MissingPos" test24,
                                     TestLabel "parseApp_ErrorProp" test25,
                                     TestLabel "parseApp_NoDir" test26,
                                     TestLabel "parseApp_L2R" test27,
                                     TestLabel "parseApp_R2L" test28,
                                     TestLabel "parseApp_OtherPos" test29]

main = defaultMain tests
