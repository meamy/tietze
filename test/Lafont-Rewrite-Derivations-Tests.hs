module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Lafont.Common
import Lafont.Graph
import Lafont.Internal.Graph
import Lafont.Rewrite.Abstraction
import Lafont.Rewrite.Common
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Internal.Abstraction
import Lafont.Rewrite.Internal.Derivations
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- Declares derivations for tests.

relx = "unknown"
rel1 = "rel1"
rel2 = "rel2"
rel3 = "rel3"
rel4 = "rel4"
rel5 = "rel5"

summaryo = DerivationSummary (RewritePreamble Nothing Nothing) [] []
summary1 = DerivationSummary (RewritePreamble (Just rel1) Nothing) [] []
summary2 = DerivationSummary (RewritePreamble (Just rel2) Nothing) [] []
summary3 = DerivationSummary (RewritePreamble (Just rel3) Nothing) [] []
summary4 = DerivationSummary (RewritePreamble (Just rel4) Nothing) [] []
summary5 = DerivationSummary (RewritePreamble (Just rel5) Nothing) [] []

rwto :: AbsRewrite
rwto = Left (Rewrite (RewriteRule [] [] True (Primitive "r1")) 0 L2R)
rwtx :: AbsRewrite
rwtx = Right (Apply relx 0 L2R)
rwt1 :: AbsRewrite
rwt1 = Right (Apply rel1 0 L2R)
rwt2 :: AbsRewrite
rwt2 = Right (Apply rel2 0 L2R)
rwt3 :: AbsRewrite
rwt3 = Right (Apply rel3 0 L2R)
rwt4 :: AbsRewrite
rwt4 = Right (Apply rel4 0 L2R)
rwt5 :: AbsRewrite
rwt5 = Right (Apply rel5 0 L2R)

derivationo = AbsDerivation summaryo [rwto, rwto, rwto, rwto, rwto, rwto]
derivation1 = AbsDerivation summaryo [rwto, rwt1, rwto, rwto, rwt3, rwto]
derivationx = AbsDerivation summaryo [rwto, rwto, rwto, rwtx, rwto, rwto]

namedDerivation1 = AbsDerivation summary1 [rwto, rwto, rwto, rwto, rwto, rwto]
namedDerivation2 = AbsDerivation summary2 [rwto, rwt1, rwt1, rwto, rwt3, rwto]
namedDerivation3 = AbsDerivation summary3 [rwto, rwto, rwto, rwt1, rwto, rwto]
namedDerivation4 = AbsDerivation summary4 [rwto, rwto, rwto, rwto, rwto, rwt3]
namedDerivation5 = AbsDerivation summary5 [rwt3, rwto, rwt4, rwto, rwto, rwto]
namedDerivationx = AbsDerivation summary1 [rwto, rwto, rwto, rwtx, rwto, rwto]
namedDerivationc = AbsDerivation summary1 [rwto, rwto, rwto, rwto, rwto, rwt2]

list0 = []
list1 = namedDerivation1:list0
list2 = namedDerivation2:list1
list3 = [namedDerivation3, namedDerivation4, namedDerivation5] ++ list2
list4 = derivationo:list3
list5 = derivation1:list4

-----------------------------------------------------------------------------------------
-- registerDerivations

reg0 = addVertex nullgraph "0"
reg1 = addVertex reg0 rel1
reg2 = addVertex reg1 rel2
reg3 = addVertex (addVertex (addVertex reg2 rel3) rel4) rel5

test1 = TestCase (assertEqual "An empty list of derivation registers a null graph."
                              (DepGraph reg0)
                              (registerDerivations list0))

test2 = TestCase (assertEqual "A single named derivation registers one vertex."
                              (DepGraph reg1)
                              (registerDerivations list1))

test3 = TestCase (assertEqual "Two named derivation register two vertices."
                              (DepGraph reg2)
                              (registerDerivations list2))

test4 = TestCase (assertEqual "Five named derivation register five vertices."
                              (DepGraph reg3)
                              (registerDerivations list3))

test5 = TestCase (assertEqual "Unnamed derivations are not added to graphs."
                              (DepGraph reg3)
                              (registerDerivations list4))

-----------------------------------------------------------------------------------------
-- addDepToGraph

reg4 = fromJust $ addEdge reg3 rel2 rel1
reg5 = fromJust $ addEdge reg4 rel2 rel3

test6 = TestCase (assertEqual "Applying a known derivation is allowed (1/2)."
                              (Right (DepGraph reg4))
                              (addDepToGraph rel2 rwt1 (DepGraph reg3)))

test7 = TestCase (assertEqual "Applying a known derivation is allowed (2/2)."
                              (Right (DepGraph reg5))
                              (addDepToGraph rel2 rwt3 (DepGraph reg4)))

test8 = TestCase (assertEqual "Using a relation is a no-ops."
                              (Right (DepGraph reg5))
                              (addDepToGraph rel2 rwto (DepGraph reg5)))

test9 = TestCase (assertEqual "Apply must be used on a known relation."
                  (Left (UnmetDep rel2 relx))
                  (addDepToGraph rel2 rwtx (DepGraph reg5)))

-----------------------------------------------------------------------------------------
-- addDepsToGraph

test10 = TestCase (assertEqual "addDepsToGraph supports empty lists of rewrites"
                               (Right (DepGraph reg3))
                               (addDepsToGraph rel2 [] (DepGraph reg3)))

test11 = TestCase (assertEqual "addDepsToGraph supports sinlgeton rewrite lists."
                               (Right (DepGraph reg4))
                               (addDepsToGraph rel2 [rwt1] (DepGraph reg3)))

test12 = TestCase (assertEqual "addDepsToGraph supports rewrite pairs."
                               (Right (DepGraph reg5))
                               (addDepsToGraph rel2 [rwt1, rwt3] (DepGraph reg3)))

test13 = TestCase (assertEqual "addDepsToGraph supports no-ops."
                               (Right (DepGraph reg5))
                               (addDepsToGraph rel2 rewrites (DepGraph reg3)))
    where rewrites = [rwto, rwt1, rwto, rwt3, rwto]

test14 = TestCase (assertEqual "addDepsToGraph requires relations are known.."
                               (Left (UnmetDep rel2 relx))
                               (addDepsToGraph rel2 rewrites (DepGraph reg5)))
    where rewrites = [rwto, rwt1, rwto, rwt3, rwto, rwtx]

-----------------------------------------------------------------------------------------
-- addDerivationToGraph

reg6 = fromJust $ addEdge reg5 rel3 rel1

test15 = TestCase (assertEqual "addDerivationToGraph supports unnamed derivations."
                               (Right (DepGraph reg3))
                               (addDerivationToGraph derivationo (DepGraph reg3)))

test16 = TestCase (assertEqual "addDerivationToGraph rejects invalid unnamed derivations."
                               (Left (UnmetDep "0" relx))
                               (addDerivationToGraph derivationx (DepGraph reg3)))

test17 = TestCase (assertEqual "addDerivationToGraph supports dep-free derivations."
                               (Right (DepGraph reg3))
                               (addDerivationToGraph namedDerivation1 (DepGraph reg3)))

test18 = TestCase (assertEqual "addDerivationToGraph supports valid derivations (1/2)."
                               (Right (DepGraph reg5))
                               (addDerivationToGraph namedDerivation2 (DepGraph reg3)))

test19 = TestCase (assertEqual "addDerivationToGraph supports valid derivations (2/2)."
                               (Right (DepGraph reg6))
                               (addDerivationToGraph namedDerivation3 (DepGraph reg5)))

test20 = TestCase (assertEqual "addDerivationToGraph rejects invalid derivations."
                               (Left (UnmetDep "rel1" relx))
                               (addDerivationToGraph namedDerivationx (DepGraph reg5)))

-----------------------------------------------------------------------------------------
-- addDerivationsToGraph

reg7 = fromJust $ addEdge reg6 rel4 rel3
reg8 = fromJust $ addEdge reg7 rel5 rel3
reg9 = fromJust $ addEdge reg8 rel5 rel4
reg10 = fromJust $ addEdge reg9 "0" rel1
reg11 = fromJust $ addEdge reg10 "0" rel3

test21 = TestCase (assertEqual "Can add edges for derivations in list0."
                               (Right (DepGraph reg3))
                               (addDerivationsToGraph list0 (DepGraph reg3)))

test22 = TestCase (assertEqual "Can add edges for derivations in list1."
                               (Right (DepGraph reg3))
                               (addDerivationsToGraph list1 (DepGraph reg3)))

test23 = TestCase (assertEqual "Can add edges for derivations in list2."
                               (Right (DepGraph reg5))
                               (addDerivationsToGraph list2 (DepGraph reg3)))

test24 = TestCase (assertEqual "Can add edges for derivations in list3."
                               (Right (DepGraph reg9))
                               (addDerivationsToGraph list3 (DepGraph reg3)))

test25 = TestCase (assertEqual "Can add edges for derivations in list4."
                               (Right (DepGraph reg9))
                               (addDerivationsToGraph list4 (DepGraph reg3)))

test26 = TestCase (assertEqual "Unnamed derivations are still recorded in the graph."
                               (Right (DepGraph reg11))
                               (addDerivationsToGraph list5 (DepGraph reg3)))

-----------------------------------------------------------------------------------------
-- detectDerivationCycle

test27 = TestCase (assertEqual "detectDerivationError returns nothing for valid graphs."
                               Nothing
                               (detectDerivationError list5))

test28 = TestCase (assertEqual "detectDerivationError identifies missing dependencies."
                               (Just (Left (UnmetDep "rel1" "unknown")))
                               (detectDerivationError (namedDerivationx:list5)))

test29 = TestCase (assertEqual "detectDerivationError identifies cycles."
                               (Just (Right cycle))
                               (detectDerivationError (namedDerivationc:list5)))
    where cycle = listToWalk ["rel1", "rel2", "rel1"]

-----------------------------------------------------------------------------------------
-- concretizeRewrite

sym1 = Symbol "a" []
sym2 = Symbol "b" []
sym3 = Symbol "c" []

lhs1 = [sym1, sym1]
lhs2 = [sym2, sym2]
rhs1 = [sym1]
rhs2 = [sym2]

ruleSummary1 = DerivationSummary (RewritePreamble (Just rel1) Nothing) lhs1 rhs1
ruleSummary2 = DerivationSummary (RewritePreamble (Just rel2) Nothing) lhs2 rhs2

eqnRule = RewriteRule lhs1 lhs1 True (Primitive "r2")
neqRule = RewriteRule lhs1 lhs1 False (Primitive "r3")

eqnRewrite = Rewrite eqnRule 0 L2R
neqRewrite = Rewrite neqRule 0 L2R

eqnDerivation = AbsDerivation ruleSummary1 [Left eqnRewrite]
neqDerivation = AbsDerivation ruleSummary2 [Left neqRewrite]

dmap = makeDerivationMap [eqnDerivation, neqDerivation]
emap = identifyEquationalRules dmap
maps = (dmap, emap)

eqnApply = Apply rel1 3 R2L
neqApply = Apply rel2 3 L2R
badApply = Apply rel2 3 R2L

eqnApplyAsRule = Rewrite (RewriteRule lhs1 rhs1 True (Derived $ Just rel1)) 3 R2L
neqApplyAsRule = Rewrite (RewriteRule lhs2 rhs2 False (Derived $ Just rel2)) 3 L2R

test30 = TestCase (assertEqual "concretizeRewrite supports rewrites (1/2)."
                               (Just eqnRewrite :: Maybe Rewrite)
                               (concretizeRewrite maps (Left eqnRewrite)))

test31 = TestCase (assertEqual "concretizeRewrite supports rewrites (2/2)."
                               (Just neqRewrite :: Maybe Rewrite)
                               (concretizeRewrite maps (Left neqRewrite)))

test32 = TestCase (assertEqual "concretizeRewrite supports equational applies."
                               (Just eqnApplyAsRule :: Maybe Rewrite)
                               (concretizeRewrite maps (Right eqnApply)))

test33 = TestCase (assertEqual "concretizeRewrite supports orientated applies."
                               (Just neqApplyAsRule :: Maybe Rewrite)
                               (concretizeRewrite maps (Right neqApply)))

test34 = TestCase (assertEqual "concretizeRewrite rejects incorrectly directed applies."
                               (Nothing :: Maybe Rewrite)
                               (concretizeRewrite maps (Right badApply)))

-----------------------------------------------------------------------------------------
-- concretizeRewrites

validRewrites = [Left eqnRewrite,
                 Right eqnApply,
                 Left eqnRewrite,
                 Right neqApply,
                 Left neqRewrite]

errorRewrites = [Left eqnRewrite,
                 Right eqnApply,
                 Left eqnRewrite,
                 Right badApply,
                 Left neqRewrite]

validRewritesResults = [eqnRewrite,
                        eqnApplyAsRule,
                        eqnRewrite,
                        neqApplyAsRule,
                        neqRewrite]

test35 = TestCase (assertEqual "concretizeRewrites supports empty lists."
                               (Right [])
                               (concretizeRewrites 0 maps []))

test36 = TestCase (assertEqual "concretizeRewrites supports valid lists of rewrites."
                               (Right validRewritesResults)
                               (concretizeRewrites 0 maps validRewrites))

test37 = TestCase (assertEqual "concretizeRewrites rejects bad applications."
                               (Left 3)
                               (concretizeRewrites 0 maps errorRewrites))

-----------------------------------------------------------------------------------------
-- concretizeDerivation

validDerivation = AbsDerivation ruleSummary1 validRewrites
errorDerivation = AbsDerivation ruleSummary2 errorRewrites

validDerivationResults = Derivation ruleSummary1 validRewritesResults

test38 = TestCase (assertEqual "concretizeDerivation supports valid lists of rewrites."
                               (Right validDerivationResults)
                               (concretizeDerivation maps validDerivation))

test39 = TestCase (assertEqual "concretizeDerivation rejects bad applications."
                               (Left 3)
                               (concretizeDerivation maps errorDerivation))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "registerDerived_Empty" test1,
                                     TestLabel "registerDerived_Single" test2,
                                     TestLabel "registerDerived_Double" test3,
                                     TestLabel "registerDerived_Many" test4,
                                     TestLabel "registerDerived_Unnamed" test5,
                                     TestLabel "addDepToGraph_Valid1" test6,
                                     TestLabel "addDepToGraph_Valid2" test7,
                                     TestLabel "addDepToGraph_Noop" test8,
                                     TestLabel "addDepToGraph_BadApply" test9,
                                     TestLabel "addDepsToGraph_Empty" test10,
                                     TestLabel "addDepsToGraph_Single" test11,
                                     TestLabel "addDepsToGraph_Double" test12,
                                     TestLabel "addDepsToGraph_Noop" test13,
                                     TestLabel "addDepsToGraph_MissingDeriv" test14,
                                     TestLabel "addDerivationToGraph_Unnamed" test15,
                                     TestLabel "addDerivationToGraph_UnnamedX" test16,
                                     TestLabel "addDerivationToGraph_Noop" test17,
                                     TestLabel "addDerivationToGraph_Valid1" test18,
                                     TestLabel "addDerivationToGraph_Valid2" test19,
                                     TestLabel "addDerivationToGraph_Missing" test20,
                                     TestLabel "addDerivationsToGraph_Empty" test21,
                                     TestLabel "addDerivationsToGraph_Singleton" test22,
                                     TestLabel "addDerivationsToGraph_Double" test23,
                                     TestLabel "addDerivationsToGraph_Many" test24,
                                     TestLabel "addDerivationsToGraph_Unnamed1" test25,
                                     TestLabel "addDerivationsToGraph_Unnamed2" test26,
                                     TestLabel "detectDerivationError_Valid" test27,
                                     TestLabel "detectDerivationError_UnmetDep" test28,
                                     TestLabel "detectDerivationError_Cycle" test29,
                                     TestLabel "concretizeRewrite_Rewrite_1" test30,
                                     TestLabel "concretizeRewrite_Rewrite_2" test31,
                                     TestLabel "concretizeRewrite_Apply_Eqn" test32,
                                     TestLabel "concretizeRewrite_Apply_Neq" test33,
                                     TestLabel "concretizeRewrite_Apply_Bad" test34,
                                     TestLabel "concretizeRewrites_Empty" test35,
                                     TestLabel "concretizeRewrites_Valid" test36,
                                     TestLabel "concretizeRewrites_Error" test37,
                                     TestLabel "concretizeDerivation_Valid" test38,
                                     TestLabel "concretizeDerivation_Error" test39]

main = defaultMain tests
