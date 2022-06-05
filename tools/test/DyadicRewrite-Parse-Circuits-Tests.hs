module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import DyadicRewrite.Common
import DyadicRewrite.Parse.Circuits

-----------------------------------------------------------------------------------------
-- parseParam

test1 = TestCase (assertEqual "parseParam rejects empty strings."
                              Nothing
                              (parseParam ""))

test2 = TestCase (assertEqual "parseParam rejects only opening brackets."
                              Nothing
                              (parseParam "["))

test3 = TestCase (assertEqual "parseParam rejects strs without closing brackets (1/2)."
                              Nothing
                              (parseParam "[10"))

test4 = TestCase (assertEqual "parseParam rejects strs without closing brackets (2/2)."
                              Nothing
                              (parseParam "[10abc"))

test5 = TestCase (assertEqual "parseParam rejects incorrect closing brackets."
                              Nothing
                              (parseParam "[10[abc"))

test6 = TestCase (assertEqual "parseParam rejects incorrect symbols inside brackets."
                              Nothing
                              (parseParam "[10abc]"))

test7 = TestCase (assertEqual "parseParam rejects negative numbers inside brackets."
                              Nothing
                              (parseParam "[-10]"))

test8 = TestCase (assertEqual "parseParam supports a single parameter."
                              (Just (10, "") :: Maybe (Int, String))
                              (parseParam "[10]"))

test9 = TestCase (assertEqual "parseParam handles post parameter data (1/2)."
                              (Just (10, "abc") :: Maybe (Int, String))
                              (parseParam "[10]abc"))

test10 = TestCase (assertEqual "parseParam handles post parameter data (1/2)."
                               (Just (20, "abc") :: Maybe (Int, String))
                               (parseParam "[20]abc"))

-----------------------------------------------------------------------------------------
-- parseParams

test11 = TestCase (assertEqual "parseParams rejects empty strings."
                              ([], "")
                              (parseParams ""))

test12 = TestCase (assertEqual "parseParams reject partial parameter lists."
                              ([], "[1")
                              (parseParams "[1"))

test13 = TestCase (assertEqual "parseParams can parse a single parameter."
                              ([1], "")
                              (parseParams "[1]"))

test14 = TestCase (assertEqual "parseParams can parse two parameter."
                               ([1, 2], "")
                               (parseParams "[1][2]"))

test15 = TestCase (assertEqual "parseParams can parse three parameter."
                               ([1, 2, 3], "")
                               (parseParams "[1][2][3]"))

test16 = TestCase (assertEqual "parseParams can parse four parameter."
                               ([1, 2, 3, 4], "")
                               (parseParams "[1][2][3][4]"))

test17 = TestCase (assertEqual "parseParams can handle unparsed postfixes."
                               ([1, 2, 3, 4], "abdfsfa[12")
                               (parseParams "[1][2][3][4]abdfsfa[12"))

-----------------------------------------------------------------------------------------
-- parseGate

test18 = TestCase (assertEqual "parseGate rejects empty strings."
                               Nothing
                               (parseGate ""))

test19 = TestCase (assertEqual "parseGate rejects bad ID's."
                               Nothing
                               (parseGate "1abc[1][2]"))

test20 = TestCase (assertEqual "parseGate rejects on missing ID's."
                               Nothing
                               (parseGate "[2]"))

test21 = TestCase (assertEqual "parseGate parses unparameterized gates."
                               (Just ((Gate "abc" []), "") :: Maybe (Gate, String))
                               (parseGate "abc"))

test22 = TestCase (assertEqual "parseGate parses unparameterized gates with postfixes."
                               (Just ((Gate "abc" []), ".") :: Maybe (Gate, String))
                               (parseGate "abc."))

test23 = TestCase (assertEqual "parseGate parses single parameter gates."
                               (Just ((Gate "abc" [1]), "") :: Maybe (Gate, String))
                               (parseGate "abc[1]"))

test24 = TestCase (assertEqual "parseGate parses two parameter gates."
                               (Just ((Gate "abc" [1, 2]), "") :: Maybe (Gate, String))
                               (parseGate "abc[1][2]"))

test25 = TestCase (assertEqual "parseGate parses three parameter gates."
                               (Just ((Gate "abc" [1, 2, 3]), "") :: Maybe (Gate, String))
                               (parseGate "abc[1][2][3]"))

test26 = TestCase (assertEqual "parseGate parses parameterized gates with postfixes."
                               (Just ((Gate "abc" [1, 2, 3]), "[4.") :: Maybe (Gate, String))
                               (parseGate "abc[1][2][3][4."))

-----------------------------------------------------------------------------------------
-- parseNonEmptyCircuit

circ1 :: Circuit
circ1 = [(Gate "abc" [])]

circ2 :: Circuit
circ2 = [(Gate "abc" []), (Gate "def" [])]

circ3 :: Circuit
circ3 = [(Gate "abc" []), (Gate "def" []), (Gate "ghi" [])]

circ4 :: Circuit
circ4 = [(Gate "abc" []), (Gate "def" [1]), (Gate "ghi" [1, 2])]

test27 = TestCase (assertEqual "parseNonEmptyCircuit rejects empty strings."
                               Nothing
                               (parseNonEmptyCircuit ""))

test28 = TestCase (assertEqual "parseNonEmptyCircuit rejects bad identifiers."
                               Nothing
                               (parseNonEmptyCircuit "1abc"))

test29 = TestCase (assertEqual "parseNonEmptyCircuit accepts single gate circuits."
                               (Just (circ1, "") :: Maybe (Circuit, String))
                               (parseNonEmptyCircuit "abc"))

test30 = TestCase (assertEqual "parseNonEmptyCircuit rejects incomplete two gate circuits."
                               Nothing
                               (parseNonEmptyCircuit "abc."))

test31 = TestCase (assertEqual "parseCircuit accepts two gate circuits."
                               (Just (circ2, "") :: Maybe (Circuit, String))
                               (parseNonEmptyCircuit "abc.def"))

test32 = TestCase (assertEqual "parseNonEmptyCircuit rejects partial three gate circuits."
                               Nothing
                               (parseNonEmptyCircuit "abc.def."))

test33 = TestCase (assertEqual "parseNonEmptyCircuit accepts three gate circuits."
                               (Just (circ3, "") :: Maybe (Circuit, String))
                               (parseNonEmptyCircuit "abc.def.ghi"))

test34 = TestCase (assertEqual "parseNonEmptyCircuit accepts parameterized circuits."
                               (Just (circ4, "") :: Maybe (Circuit, String))
                               (parseNonEmptyCircuit "abc.def[1].ghi[1][2]"))

test35 = TestCase (assertEqual "parseNonEmptyCircuit accepts postfixes."
                               (Just (circ4, " .asd") :: Maybe (Circuit, String))
                               (parseNonEmptyCircuit "abc.def[1].ghi[1][2] .asd"))

test36 = TestCase (assertEqual "parseNonEmptyCircuit requires that strings terminate."
                               Nothing
                               (parseNonEmptyCircuit "abc.def[1].ghi[1][2]asd"))

-----------------------------------------------------------------------------------------
-- parseCircuit

test38 = TestCase (assertEqual "parseCircuit supports empty strings."
                               (Just (circ4, " .asd") :: Maybe (Circuit, String))
                               (parseCircuit "abc.def[1].ghi[1][2] .asd"))

test39 = TestCase (assertEqual "parseCircuit supports non-empty strings."
                               (Just ([], ".asd") :: Maybe (Circuit, String))
                               (parseCircuit "ε.asd"))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseParam_EmptyString" test1,
                                     TestLabel "parseParam_OnlyOpenBracket" test2,
                                     TestLabel "parseParam_NoClosingBracket_Test0" test3,
                                     TestLabel "parseParam_NoClosingBracket_Test1" test4,
                                     TestLabel "parseParam_WrongClosingBracket" test5,
                                     TestLabel "parseParam_BadNumber" test6,
                                     TestLabel "parseParam_NegativeNumber" test7,
                                     TestLabel "parseParam_Valid_Test0" test8,
                                     TestLabel "parseParam_Valid_Test1" test9,
                                     TestLabel "parseParam_Valid_Test2" test10,
                                     TestLabel "parseParams_EmptyString" test11,
                                     TestLabel "parseParams_NotFullParam" test12,
                                     TestLabel "parseParams_OneParam" test13,
                                     TestLabel "parseParams_TwoParams" test14,
                                     TestLabel "parseParams_ThreeParams" test15,
                                     TestLabel "parseParams_FourParams" test16,
                                     TestLabel "parseParams_PostString" test17,
                                     TestLabel "parseGate_EmptyString" test18,
                                     TestLabel "parseGate_BadID" test19,
                                     TestLabel "parseGate_NoID" test20,
                                     TestLabel "parseGate_NoParams" test21,
                                     TestLabel "parseGate_NoParamsPostString" test22,
                                     TestLabel "parseGate_OneParam" test23,
                                     TestLabel "parseGate_TwoParams" test24,
                                     TestLabel "parseGate_ThreeParams" test25,
                                     TestLabel "parseGate_ParamsPostString" test26,
                                     TestLabel "parseNonEmptyCircuit_EmptyString" test27,
                                     TestLabel "parseNonEmptyCircuit_BadID" test28,
                                     TestLabel "parseNonEmptyCircuit_OneGate" test29,
                                     TestLabel "parseNonEmptyCircuit_OneGateBadDot" test30,
                                     TestLabel "parseNonEmptyCircuit_TwoGates" test31,
                                     TestLabel "parseNonEmptyCircuit_TwoGatesBadDot" test32,
                                     TestLabel "parseNonEmptyCircuit_ThreeGates" test33,
                                     TestLabel "parseNonEmptyCircuit_ParamGates" test34,
                                     TestLabel "parseNonEmptyCircuit_PostString" test35,
                                     TestLabel "parseNonEmptyCircuit_MissingSpace" test36,
                                     TestLabel "parseCircuit_NonEmpty" test38,
                                     TestLabel "parseCircuit_Empty" test39]

main = defaultMain tests
