module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Either
import Tietze.Common
import Tietze.Rewrite.Rules
import Tietze.Rewrite.Lookup
import Tietze.Parse.Common
import Tietze.Parse.Internal.RelationFile
import Tietze.Parse.MonWords
import Tietze.Parse.RelationFile

-----------------------------------------------------------------------------------------
-- parseRule

word1a :: MonWord
word1a = [(Symbol "a1" []), (Symbol "a2" []), (Symbol "a3" [])]

word2a :: MonWord
word2a = [(Symbol "abc" []), (Symbol "def" [1]), (Symbol "ghi" [1, 2])]

word1b :: MonWord
word1b = [(Symbol "a1" []), (Symbol "a2" [])]

word2b :: MonWord
word2b = [(Symbol "abc" []), (Symbol "def" [1]), (Symbol "ghi" [1, 2]), (Symbol "a3" [])]

test1 = TestCase (assertEqual "Tests that empty strings are rejected."
                              (Left (Right RuleMissingLHS))
                              (parseRule "id" ""))

test2 = TestCase (assertEqual "Tests that missing left-hand sides are rejected."
                              (Left (Right RuleMissingLHS))
                              (parseRule "id" "1abc"))

test3 = TestCase (assertEqual "Tests that missing rule types are detected."
                              (Left (Right (InvalidRuleType 8)))
                              (parseRule "id" "a1.a2.a3"))

test4 = TestCase (assertEqual "Tests that bad rule types are detected."
                              (Left (Right (InvalidRuleType 8)))
                              (parseRule "id" "a1.a2.a3 #"))

test5 = TestCase (assertEqual "Tests that missing right-hand sides are rejected."
                              (Left (Right RuleMissingRHS))
                              (parseRule "id" "a1.a2.a3 ="))

test6 = TestCase (assertEqual "Tests bad right-hand sides are rejected."
                              (Left (Right RuleMissingRHS))
                              (parseRule "id" "a1.a2.a3 = 1abc"))

test7 = TestCase (assertEqual "Tests bad right-hand sides are rejected."
                              (Left (Left (UnexpectedSymbol 31)))
                              (parseRule "id" "a1.a2.a3 = abc.def[1].ghi[1][2]  bad"))

test8 = TestCase (assertEqual "Tests that equational rules are parsed (1/2)."
                              (Right (RewriteRule word1a word2a True (Primitive "r1")))
                              (parseRule "r1" "a1.a2.a3 = abc.def[1].ghi[1][2] "))

test9 = TestCase (assertEqual "Tests that equational rules are parsed (2/2)."
                              (Right (RewriteRule word1b word2b True (Primitive "r2")))
                              (parseRule "r2" "a1.a2 = abc.def[1].ghi[1][2].a3 "))

test10 = TestCase (assertEqual "Tests that production rules are parsed (1/2)."
                               (Right (RewriteRule word1a word2a False (Primitive "r3")))
                               (parseRule "r3" "a1.a2.a3 → abc.def[1].ghi[1][2] "))

test11 = TestCase (assertEqual "Tests that production rules are parsed (2/2)."
                               (Right (RewriteRule word1b word2b False (Primitive "r4")))
                               (parseRule "r4" "a1.a2 → abc.def[1].ghi[1][2].a3 "))

test12 = TestCase (assertEqual "Tests that rules between empty strings are parsed."
                               (Right (RewriteRule [] [] False (Primitive "r5")))
                               (parseRule "r5" "ε → ε "))

-----------------------------------------------------------------------------------------
-- parseRuleDefn

test13 = TestCase (assertEqual "Tests that bad rule names are rejected."
                               (Left (Right InvalidRuleName))
                               (parseRuleDefn "1ab ε → ε"))

test14 = TestCase (assertEqual "Tests that bad rules are rejected (no position)."
                               (Left (Right RuleMissingRHS))
                               (parseRuleDefn "rel1 a1.a2.a3 ="))

test15 = TestCase (assertEqual "Tests that bad rules are rejected (positional)."
                               (Left (Left (UnexpectedSymbol 18)))
                               (parseRuleDefn "rel1 a.b.c = b.c.a  x"))

test16 = TestCase (assertEqual "Tests that spaces are required after an identifier."
                               (Left (Left (UnexpectedSymbol 4)))
                               (parseRuleDefn "rel1ε → ε"))

test17 = TestCase (assertEqual "Tests that equational rules are parsed (1/2)."
                               (Right ("rel1", (RewriteRule word1a word1b True (Primitive "rel1"))))
                               (parseRuleDefn "  rel1 a1.a2.a3   =  a1.a2  "))

test18 = TestCase (assertEqual "Tests that equational rules are parsed (2/2)."
                               (Right ("rel2", (RewriteRule word1b word1a True (Primitive "rel2"))))
                               (parseRuleDefn "  rel2   a1.a2   =  a1.a2.a3  "))

-----------------------------------------------------------------------------------------
-- findUnknownGenInMonWord

symbWP1 :: Symbol
symbWP1 = Symbol "aaa" [1, 2]

symbWP2 :: Symbol
symbWP2 = Symbol "bbb" [1, 2, 3]

symbWP3 :: Symbol
symbWP3 = Symbol "ccc" [1, 2, 3, 4]

symbWOP1 :: Symbol
symbWOP1 = Symbol "ddd" []

symbWOP2 :: Symbol
symbWOP2 = Symbol "eee" []

symbWOP3 :: Symbol
symbWOP3 = Symbol "fff" []

paramWord :: MonWord
paramWord = [symbWOP1, symbWP1, symbWOP2, symbWP2, symbWOP3, symbWP3]

noParamWord :: MonWord
noParamWord = [symbWOP1, symbWOP1, symbWOP2, symbWOP2, symbWOP3, symbWOP3]

genList1 :: [String]
genList1 = [(name symbWOP1), (name symbWOP2)]
genList2 = (name symbWOP3):genList1
genList3 = (name symbWP1):(name symbWP2):(name symbWP3):genList2

test19 = TestCase (assertEqual "findUnknownGenInMonWord supports empty strings."
                               Nothing
                               (findUnknownGenInMonWord genList3 []))

test20 = TestCase (assertEqual "findUnknownGenInMonWord rejects parameters."
                               (Just symbWP1)
                               (findUnknownGenInMonWord genList3 paramWord))

test21 = TestCase (assertEqual "findUnknownGenInMonWord rejects unlisted generator."
                               (Just symbWOP3)
                               (findUnknownGenInMonWord genList1 noParamWord))

test22 = TestCase (assertEqual "findUnknownGenInMonWord accepts strings of generators."
                               Nothing
                               (findUnknownGenInMonWord genList2 noParamWord))

-----------------------------------------------------------------------------------------
-- findUnknownGenInRule

badParamRuleLHS :: RewriteRule
badParamRuleLHS = RewriteRule paramWord [] True (Primitive "xyz")

badNoParamRuleLHS :: RewriteRule
badNoParamRuleLHS = RewriteRule noParamWord [] True (Primitive "x123")

badParamRuleRHS :: RewriteRule
badParamRuleRHS = RewriteRule [] paramWord True (Primitive "r9")

badNoParamRuleRHS :: RewriteRule
badNoParamRuleRHS = RewriteRule [] noParamWord True (Primitive "r10")

goodRule :: RewriteRule
goodRule = RewriteRule noParamWord noParamWord True (Primitive "abc")

test23 = TestCase (assertEqual "findUnknownGenInRule detects bad symbols on the LHS (1/2)."
                               (Just symbWP1)
                               (findUnknownGenInRule genList3 badParamRuleLHS))

test24 = TestCase (assertEqual "findUnknownGenInRule detects bad symbols on the LHS (2/2)."
                               (Just symbWOP3)
                               (findUnknownGenInRule genList1 badNoParamRuleLHS))

test25 = TestCase (assertEqual "findUnknownGenInRule detects bad symbols on the RHS (1/2)."
                               (Just symbWP1)
                               (findUnknownGenInRule genList3 badParamRuleRHS))

test26 = TestCase (assertEqual "findUnknownGenInRule detects bad symbols on the RHS (2/2)."
                               (Just symbWOP3)
                               (findUnknownGenInRule genList1 badNoParamRuleRHS))

test27 = TestCase (assertEqual "findUnknownGenInRule accepts valid rules."
                               Nothing
                               (findUnknownGenInRule genList2 goodRule))

-----------------------------------------------------------------------------------------
-- updateRules

rel1 :: (String, RewriteRule)
rel1 = ("xyx", badParamRuleLHS)

rel2 :: (String, RewriteRule)
rel2 = ("x123", badNoParamRuleLHS)

dupRule :: (String, RewriteRule)
dupRule = ("abc", goodRule)

emptyDict :: RuleDict
emptyDict = empty

dupDict = addRule emptyDict dupRule
dict1 = addRule emptyDict rel1
dict2 = addRule dict1 rel2

symbA :: Symbol
symbA = Symbol "a" []

symbB :: Symbol
symbB = Symbol "b" []

addedRule :: RewriteRule
addedRule = RewriteRule [symbA, symbB] [symbB, symbA] True (Primitive "abc")

test28 = TestCase (assertEqual "Tests that updateRules propogates errors (1/2)."
                               (Left (Right InvalidRuleName))
                               (updateRules emptyDict [] "1abc"))

test29 = TestCase (assertEqual "Tests that updateRules propogates errors (2/2)."
                               (Left (Right (InvalidRuleType 12)))
                               (updateRules emptyDict [] "abc a1.a2.a3"))

test30 = TestCase (assertEqual "Tests that updateRules validates generators (1/2)."
                               (Left (Right (UnknownGenName "c")))
                               (updateRules emptyDict ["a", "b"] "abc a.b = c.b.a"))

test31 = TestCase (assertEqual "Tests that updateRules validates generators (2/2)."
                               (Left (Right (UnknownGenName "b[1]")))
                               (updateRules emptyDict ["a", "b"] "abc a = b[1].b"))

test32 = TestCase (assertEqual "Tests that updateRules detects duplicate names."
                               (Left (Right (DuplicateRuleName "abc")))
                               (updateRules dupDict ["a", "b"] " abc  a.b  = b.a  "))

test33 = TestCase (assertEqual "Tests that updateRules adds new rules."
                               (Just addedRule)
                               rel)
         where rel = case (updateRules emptyDict ["a", "b"] "abc a.b = b.a") of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "abc"

test34 = TestCase (assertEqual "Tests that updateRules preserves generators (1/2)."
                               (Just (snd rel1))
                               rel)
         where rel = case (updateRules dict2 ["a", "b"] "abc a.b = b.a") of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict (fst rel1)

test35 = TestCase (assertEqual "Tests that updateRules preserves generators (2/2)."
                               (Just (snd rel2))
                               rel)
         where rel = case (updateRules dict2 ["a", "b"] "abc a.b = b.a") of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict (fst rel2)

-----------------------------------------------------------------------------------------
-- parseRelFile

gens :: [String]
gens = ["a", "b"]

fileWord1 :: MonWord
fileWord1 = [(Symbol "a" []), (Symbol "b" [])]

fileWord2 :: MonWord
fileWord2 = [(Symbol "b" []), (Symbol "a" [])]

fileRel1 :: RewriteRule
fileRel1 = RewriteRule fileWord1 fileWord2 True (Primitive "abc")

-- Single line tests.

test36 = TestCase (assertEqual "Tests parsing of a single rule with parseRelFile."
                               (Just fileRel1 :: Maybe RewriteRule)
                               rel)
         where rel = case (parseRelFile empty gens ["  abc  a.b =  b.a -- comment"] 0) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "abc"

test37 = TestCase (assertEqual "Tests that parseRelFile handles single line errors (1/2)."
                               (Left (0, (Left (UnexpectedSymbol 14))))
                               (parseRelFile empty gens ["   abc x  =  y   xyz -- comment"] 0))

test45 = TestCase (assertEqual "Tests that parseRelFile handles single line errors (2/2)."
                               (Left (0, (Right (UnknownGenName "c"))))
                               (parseRelFile empty gens ["  abc  a.b  =  c.b.a -- comment"] 0))

-- Multi-line tests.

validMultiline :: [String]
validMultiline = ["", "abc a.b = b.a", "cdf a.b = ε ", "", "xyz_123 ε → b.a"]

fileRel2 :: RewriteRule
fileRel2 = RewriteRule fileWord1 [] True (Primitive "cdf")

fileRel3 :: RewriteRule
fileRel3 = RewriteRule [] fileWord2 False (Primitive "xyz_123")

test38 = TestCase (assertEqual "Tests parsing of a single rule with multiple lines."
                               (Just fileRel1 :: Maybe RewriteRule)
                               rel)
         where input =  [" \t\t\t  \t", "  \t\t", "abc a.b = b.a", "  -- comment"]
               rel = case (parseRelFile empty gens input 0) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "abc"

test39 = TestCase (assertEqual "Tests parsing of multiple valid rules (1/3)."
                               (Just fileRel1 :: Maybe RewriteRule)
                               rel)
         where rel = case (parseRelFile empty gens validMultiline 0) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "abc"

test40 = TestCase (assertEqual "Tests parsing of multiple valid rules (2/3)."
                               (Just fileRel2 :: Maybe RewriteRule)
                               rel)
         where rel = case (parseRelFile empty gens validMultiline 0) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "cdf"

test41 = TestCase (assertEqual "Tests parsing of multiple valid rules (3/3)."
                               (Just fileRel3 :: Maybe RewriteRule)
                               rel)
         where rel = case (parseRelFile empty gens validMultiline 0) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "xyz_123"

test42 = TestCase (assertEqual "Tests that parseRelFile handles errors after valid lines."
                               (Left (1, (Right (DuplicateRuleName "r2"))))
                               (parseRelFile empty gens input 0))
         where input = ["r1 a = a", "r2 b = b", "r3 a = a", "r2 b = b", "r4 a = a"]

-- Adjusted starting line.

test43 = TestCase (assertEqual "Tests that parseRelFile handles offsets (1/2)."
                               (Just fileRel1 :: Maybe RewriteRule)
                               rel)
         where rel = case (parseRelFile empty gens ["", "", "", "abc a.b = b.a", "", ""] 5) of
                         Left _     -> Nothing
                         Right dict -> interpretRule dict "abc"

test44 = TestCase (assertEqual "Tests that parseRelFile handles offsets (2/2)."
                               (Left (8, (Right InvalidRuleName)))
                               (parseRelFile empty gens ["", "", "", "1a a = a", "", ""] 5))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseRule_EmptyString" test1,
                                     TestLabel "parseRule_NoLHS" test2,
                                     TestLabel "parseRule_NoRuleOp" test3,
                                     TestLabel "parseRule_BadRuleOp" test4,
                                     TestLabel "parseRule_NoRHS" test5,
                                     TestLabel "parseRule_BadRHS" test6,
                                     TestLabel "parseRule_BadEndOfLine" test7,
                                     TestLabel "parseRule_EquationalOne" test8,
                                     TestLabel "parseRule_EquationalTwo" test9,
                                     TestLabel "parseRule_ProductionOne" test10,
                                     TestLabel "parseRule_ProductionTwo" test11,
                                     TestLabel "parseRule_EmptyStrings" test12,
                                     TestLabel "parseRuleDefn_BadName" test13,
                                     TestLabel "parseRuleDefn_NoPosRuleErr" test14,
                                     TestLabel "parseRuleDefn_PosRuleErr" test15,
                                     TestLabel "parseRuleDefn_SpaceAfterID" test16,
                                     TestLabel "parseRuleDefn_GoodRuleOne" test17,
                                     TestLabel "parseRuleDefn_GoodRuleTwo" test18,
                                     TestLabel "findUnknownGenInMonWord_Empty" test19,
                                     TestLabel "findUnknownGenInMonWord_Params" test20,
                                     TestLabel "findUnknownGenInMonWord_BadName" test21,
                                     TestLabel "findUnknownGenInMonWord_Accepts" test22,
                                     TestLabel "findUnknownGenInRule_ParamsLHS" test23,
                                     TestLabel "findUnknownGenInRule_BadNameLHS" test24,
                                     TestLabel "findUnknownGenInRule_ParamsRHS" test25,
                                     TestLabel "findUnknownGenInRule_BadNameRHS" test26,
                                     TestLabel "findUnknownGenInRule_GoodRule" test27,
                                     TestLabel "updateRules_PropogateOne" test28,
                                     TestLabel "updateRules_PropogateTwo" test29,
                                     TestLabel "updateRules_BadGenOne" test30,
                                     TestLabel "updateRules_BadGenTwo" test31,
                                     TestLabel "updateRules_DupRuleName" test32,
                                     TestLabel "updateRules_AddNewRule" test33,
                                     TestLabel "updateRules_UpdateRulesOne" test34,
                                     TestLabel "updateRules_UpdateRulesTwo" test35,
                                     TestLabel "parseRelFile_ValidRule" test36,
                                     TestLabel "parseRelFile_InvalidRuleOne" test37,
                                     TestLabel "parseRelFile_ValidRuleMultiline" test38,
                                     TestLabel "parseRelFile_MultipleRulesOne" test39,
                                     TestLabel "parseRelFile_MultipleRulesTwo" test40,
                                     TestLabel "parseRelFile_MultipleRulesThree" test41,
                                     TestLabel "parseRelFile_MidParsingError" test42,
                                     TestLabel "parseRelFile_OffsetValid" test43,
                                     TestLabel "parseRelFile_OffsetInvalid" test44,
                                     TestLabel "parseRelFile_InvalidRuleTwo" test45]

main = defaultMain tests
