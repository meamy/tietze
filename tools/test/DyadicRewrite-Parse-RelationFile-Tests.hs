module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Data.Either
import DyadicRewrite.Common
import DyadicRewrite.Rewrite.Rules
import DyadicRewrite.Rewrite.Lookup
import DyadicRewrite.Parse.Common
import DyadicRewrite.Parse.RelationFile

-----------------------------------------------------------------------------------------
-- parseRelation

circ1a :: Circuit
circ1a = [(Gate "a1" []), (Gate "a2" []), (Gate "a3" [])]

circ2a :: Circuit
circ2a = [(Gate "abc" []), (Gate "def" [1]), (Gate "ghi" [1, 2])]

circ1b :: Circuit
circ1b = [(Gate "a1" []), (Gate "a2" [])]

circ2b :: Circuit
circ2b = [(Gate "abc" []), (Gate "def" [1]), (Gate "ghi" [1, 2]), (Gate "a3" [])]

test1 = TestCase (assertEqual "Tests that empty strings are rejected."
                              (Left (Right RelMissingLHS))
                              (parseRelation ""))

test2 = TestCase (assertEqual "Tests that missing left-hand sides are rejected."
                              (Left (Right RelMissingLHS))
                              (parseRelation "1abc"))

test3 = TestCase (assertEqual "Tests that missing relation operations are detected."
                              (Left (Right (InvalidRelType 8)))
                              (parseRelation "a1.a2.a3"))

test4 = TestCase (assertEqual "Tests that bad relation types are detected."
                              (Left (Right (InvalidRelType 8)))
                              (parseRelation "a1.a2.a3 #"))

test5 = TestCase (assertEqual "Tests that missing right-hand sides are rejected."
                              (Left (Right RelMissingRHS))
                              (parseRelation "a1.a2.a3 ="))

test6 = TestCase (assertEqual "Tests bad right-hand sides are rejected."
                              (Left (Right RelMissingRHS))
                              (parseRelation "a1.a2.a3 = 1abc"))

test7 = TestCase (assertEqual "Tests bad right-hand sides are rejected."
                              (Left (Left (UnexpectedSymbol 31)))
                              (parseRelation "a1.a2.a3 = abc.def[1].ghi[1][2]  bad"))

test8 = TestCase (assertEqual "Tests that equational relations are parsed (1/2)."
                              (Right (RewriteRule circ1a circ2a True False))
                              (parseRelation "a1.a2.a3 = abc.def[1].ghi[1][2] "))

test9 = TestCase (assertEqual "Tests that equational relations are parsed (2/2)."
                              (Right (RewriteRule circ1b circ2b True False))
                              (parseRelation "a1.a2 = abc.def[1].ghi[1][2].a3 "))

test10 = TestCase (assertEqual "Tests that production rules are parsed (1/2)."
                               (Right (RewriteRule circ1a circ2a False False))
                               (parseRelation "a1.a2.a3 → abc.def[1].ghi[1][2] "))

test11 = TestCase (assertEqual "Tests that production rules are parsed (2/2)."
                               (Right (RewriteRule circ1b circ2b False False))
                               (parseRelation "a1.a2 → abc.def[1].ghi[1][2].a3 "))

test12 = TestCase (assertEqual "Tests that relations between empty strings are parsed."
                               (Right (RewriteRule [] [] False False))
                               (parseRelation "ε → ε "))

-----------------------------------------------------------------------------------------
-- parseRelationDefn

test13 = TestCase (assertEqual "Tests that bad relation names are rejected."
                               (Left (Right InvalidRelName))
                               (parseRelationDefn "1ab ε → ε"))

test14 = TestCase (assertEqual "Tests that bad relations are rejected (no position)."
                               (Left (Right RelMissingRHS))
                               (parseRelationDefn "rel1 a1.a2.a3 ="))

test15 = TestCase (assertEqual "Tests that bad relations are rejected (positional)."
                               (Left (Left (UnexpectedSymbol 18)))
                               (parseRelationDefn "rel1 a.b.c = b.c.a  x"))

test16 = TestCase (assertEqual "Tests that spaces are required after an identifier."
                               (Left (Left (UnexpectedSymbol 4)))
                               (parseRelationDefn "rel1ε → ε"))

test17 = TestCase (assertEqual "Tests that equational relations are parsed (1/2)."
                               (Right ("rel1", (RewriteRule circ1a circ1b True False)))
                               (parseRelationDefn "  rel1 a1.a2.a3   =  a1.a2  "))

test18 = TestCase (assertEqual "Tests that equational relations are parsed (2/2)."
                               (Right ("rel2", (RewriteRule circ1b circ1a True False)))
                               (parseRelationDefn "  rel2   a1.a2   =  a1.a2.a3  "))

-----------------------------------------------------------------------------------------
-- findUnknownGenInCircuit

gateWP1 :: Gate
gateWP1 = Gate "aaa" [1, 2]

gateWP2 :: Gate
gateWP2 = Gate "bbb" [1, 2, 3]

gateWP3 :: Gate
gateWP3 = Gate "ccc" [1, 2, 3, 4]

gateWOP1 :: Gate
gateWOP1 = Gate "ddd" []

gateWOP2 :: Gate
gateWOP2 = Gate "eee" []

gateWOP3 :: Gate
gateWOP3 = Gate "fff" []

paramCirc :: Circuit
paramCirc = [gateWOP1, gateWP1, gateWOP2, gateWP2, gateWOP3, gateWP3]

noParamCirc :: Circuit
noParamCirc = [gateWOP1, gateWOP1, gateWOP2, gateWOP2, gateWOP3, gateWOP3]

genList1 :: [String]
genList1 = [(name gateWOP1), (name gateWOP2)]
genList2 = (name gateWOP3):genList1
genList3 = (name gateWP1):(name gateWP2):(name gateWP3):genList2

test19 = TestCase (assertEqual "findUnknownGenInCircuit supports empty strings."
                               Nothing
                               (findUnknownGenInCircuit genList3 []))

test20 = TestCase (assertEqual "findUnknownGenInCircuit rejects parameters."
                               (Just gateWP1)
                               (findUnknownGenInCircuit genList3 paramCirc))

test21 = TestCase (assertEqual "findUnknownGenInCircuit rejects unlisted generator."
                               (Just gateWOP3)
                               (findUnknownGenInCircuit genList1 noParamCirc))

test22 = TestCase (assertEqual "findUnknownGenInCircuit accepts strings of generators."
                               Nothing
                               (findUnknownGenInCircuit genList2 noParamCirc))

-----------------------------------------------------------------------------------------
-- findUnknownGenInRel

badParamRelLHS :: RewriteRule
badParamRelLHS = RewriteRule paramCirc [] True False

badNoParamRelLHS :: RewriteRule
badNoParamRelLHS = RewriteRule noParamCirc [] True False

badParamRelRHS :: RewriteRule
badParamRelRHS = RewriteRule [] paramCirc True False

badNoParamRelRHS :: RewriteRule
badNoParamRelRHS = RewriteRule [] noParamCirc True False

goodRel :: RewriteRule
goodRel = RewriteRule noParamCirc noParamCirc True False

test23 = TestCase (assertEqual "findUnknownGenInRel detects bad gates on the LHS (1/2)."
                               (Just gateWP1)
                               (findUnknownGenInRel genList3 badParamRelLHS))

test24 = TestCase (assertEqual "findUnknownGenInRel detects bad gates on the LHS (2/2)."
                               (Just gateWOP3)
                               (findUnknownGenInRel genList1 badNoParamRelLHS))

test25 = TestCase (assertEqual "findUnknownGenInRel detects bad gates on the RHS (1/2)."
                               (Just gateWP1)
                               (findUnknownGenInRel genList3 badParamRelRHS))

test26 = TestCase (assertEqual "findUnknownGenInRel detects bad gates on the RHS (2/2)."
                               (Just gateWOP3)
                               (findUnknownGenInRel genList1 badNoParamRelRHS))

test27 = TestCase (assertEqual "findUnknownGenInRel accepts valid relations."
                               Nothing
                               (findUnknownGenInRel genList2 goodRel))

-----------------------------------------------------------------------------------------
-- updateRelations

rel1 :: (String, RewriteRule)
rel1 = ("xyx", badParamRelLHS)

rel2 :: (String, RewriteRule)
rel2 = ("x123", badNoParamRelLHS)

dupRel :: (String, RewriteRule)
dupRel = ("abc", goodRel)

emptyDict :: RelDict
emptyDict = empty

dupDict = addRel emptyDict dupRel
dict1 = addRel emptyDict rel1
dict2 = addRel dict1 rel2

gateA :: Gate
gateA = Gate "a" []

gateB :: Gate
gateB = Gate "b" []

addedRel :: RewriteRule
addedRel = RewriteRule [gateA, gateB] [gateB, gateA] True False

test28 = TestCase (assertEqual "Tests that updateRelations propogates errors (1/2)."
                               (Left (Right InvalidRelName))
                               (updateRelations emptyDict [] "1abc"))

test29 = TestCase (assertEqual "Tests that updateRelations propogates errors (2/2)."
                               (Left (Right (InvalidRelType 12)))
                               (updateRelations emptyDict [] "abc a1.a2.a3"))

test30 = TestCase (assertEqual "Tests that updateRelations validates generators (1/2)."
                               (Left (Right (UnknownGenName "c")))
                               (updateRelations emptyDict ["a", "b"] "abc a.b = c.b.a"))

test31 = TestCase (assertEqual "Tests that updateRelations validates generators (2/2)."
                               (Left (Right (UnknownGenName "b[1]")))
                               (updateRelations emptyDict ["a", "b"] "abc a = b[1].b"))

test32 = TestCase (assertEqual "Tests that updateRelations detects duplicate names."
                               (Left (Right (DuplicateRelName "abc")))
                               (updateRelations dupDict ["a", "b"] " abc  a.b  = b.a  "))

test33 = TestCase (assertEqual "Tests that updateRelations adds new relations."
                               (Just addedRel)
                               rel)
         where rel = case (updateRelations emptyDict ["a", "b"] "abc a.b = b.a") of
                         Left err   -> Nothing
                         Right dict -> interpretRel dict "abc"

test34 = TestCase (assertEqual "Tests that updateRelations preserves generators (1/2)."
                               (Just (snd rel1))
                               rel)
         where rel = case (updateRelations dict2 ["a", "b"] "abc a.b = b.a") of
                         Left err   -> Nothing
                         Right dict -> interpretRel dict (fst rel1)

test35 = TestCase (assertEqual "Tests that updateRelations preserves generators (2/2)."
                               (Just (snd rel2))
                               rel)
         where rel = case (updateRelations dict2 ["a", "b"] "abc a.b = b.a") of
                         Left err   -> Nothing
                         Right dict -> interpretRel dict (fst rel2)

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "parseRelation_EmptyString" test1,
                                     TestLabel "parseRelation_NoLHS" test2,
                                     TestLabel "parseRelation_NoRelOp" test3,
                                     TestLabel "parseRelation_BadRelOp" test4,
                                     TestLabel "parseRelation_NoRHS" test5,
                                     TestLabel "parseRelation_BadRHS" test6,
                                     TestLabel "parseRelation_BadEndOfLine" test7,
                                     TestLabel "parseRelation_EquationalOne" test8,
                                     TestLabel "parseRelation_EquationalTwo" test9,
                                     TestLabel "parseRelation_ProductionOne" test10,
                                     TestLabel "parseRelation_ProductionTwo" test11,
                                     TestLabel "parseRelation_EmptyStrings" test12,
                                     TestLabel "parseRelationDefn_BadName" test13,
                                     TestLabel "parseRelationDefn_NoPosRelErr" test14,
                                     TestLabel "parseRelationDefn_PosRelErr" test15,
                                     TestLabel "parseRelationDefn_SpaceAfterID" test16,
                                     TestLabel "parseRelationDefn_GoodRelOne" test17,
                                     TestLabel "parseRelationDefn_GoodRelTwo" test18,
                                     TestLabel "findUnknownGenInCircuit_Empty" test19,
                                     TestLabel "findUnknownGenInCircuit_Params" test20,
                                     TestLabel "findUnknownGenInCircuit_BadName" test21,
                                     TestLabel "findUnknownGenInCircuit_Accepts" test22,
                                     TestLabel "findUnknownGenInRel_ParamsLHS" test23,
                                     TestLabel "findUnknownGenInRel_BadNameLHS" test24,
                                     TestLabel "findUnknownGenInRel_ParamsRHS" test25,
                                     TestLabel "findUnknownGenInRel_BadNameRHS" test26,
                                     TestLabel "findUnknownGenInRel_GoodRel" test27,
                                     TestLabel "updateRelations_PropogateOne" test28,
                                     TestLabel "updateRelations_PropogateTwo" test29,
                                     TestLabel "updateRelations_BadGenOne" test30,
                                     TestLabel "updateRelations_BadGenTwo" test31,
                                     TestLabel "updateRelations_DupRelName" test32,
                                     TestLabel "updateRelations_AddNewRel" test33,
                                     TestLabel "updateRelations_UpdateRelsOne" test34,
                                     TestLabel "updateRelations_UpdateRelsTwo" test35]

main = defaultMain tests
