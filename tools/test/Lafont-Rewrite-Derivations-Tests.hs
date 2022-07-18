module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Lafont.Graph
import Lafont.Rewrite.Derivations
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

summaryo = DerivationSummary (RewritePreamble Nothing) [] []
summary1 = DerivationSummary (RewritePreamble (Just rel1)) [] []
summary2 = DerivationSummary (RewritePreamble (Just rel2)) [] []
summary3 = DerivationSummary (RewritePreamble (Just rel3)) [] []
summary4 = DerivationSummary (RewritePreamble (Just rel4)) [] []
summary5 = DerivationSummary (RewritePreamble (Just rel5)) [] []

rwto = Rewrite (RewriteRule [] [] True Nothing) 0 True
rwtx = Rewrite (RewriteRule [] [] True (Just relx)) 0 True
rwt1 = Rewrite (RewriteRule [] [] True (Just rel1)) 0 True
rwt2 = Rewrite (RewriteRule [] [] True (Just rel2)) 0 True
rwt3 = Rewrite (RewriteRule [] [] True (Just rel3)) 0 True
rwt4 = Rewrite (RewriteRule [] [] True (Just rel4)) 0 True
rwt5 = Rewrite (RewriteRule [] [] True (Just rel5)) 0 True

derivationo = Derivation summaryo [rwto, rwto, rwto, rwto, rwto, rwto]
derivation1 = Derivation summaryo [rwto, rwt1, rwto, rwto, rwt3, rwto]
derivationx = Derivation summaryo [rwto, rwto, rwto, rwtx, rwto, rwto]

namedDerivation1 = Derivation summary1 [rwto, rwto, rwto, rwto, rwto, rwto]
namedDerivation2 = Derivation summary2 [rwto, rwt1, rwt1, rwto, rwt3, rwto]
namedDerivation3 = Derivation summary3 [rwto, rwto, rwto, rwt1, rwto, rwto]
namedDerivation4 = Derivation summary4 [rwto, rwto, rwto, rwto, rwto, rwt3]
namedDerivation5 = Derivation summary5 [rwt3, rwto, rwt4, rwto, rwto, rwto]
namedDerivationx = Derivation summary1 [rwto, rwto, rwto, rwtx, rwto, rwto]
namedDerivationc = Derivation summary1 [rwto, rwto, rwto, rwto, rwto, rwt2]

list0 = []
list1 = namedDerivation1:list0
list2 = namedDerivation2:list1
list3 = [namedDerivation3, namedDerivation4, namedDerivation5] ++ list2
list4 = derivationo:list3
list5 = derivation1:list4

-----------------------------------------------------------------------------------------
-- registerDerivations

reg0 = addVertex nullgraph ""
reg1 = addVertex reg0 rel1
reg2 = addVertex reg1 rel2
reg3 = addVertex (addVertex (addVertex reg2 rel3) rel4) rel5

test1 = TestCase (assertEqual "An empty list of derivation registers a null graph."
                              reg0
                              (registerDerivations list0))

test2 = TestCase (assertEqual "A single named derivation registers one vertex."
                              reg1
                              (registerDerivations list1))

test3 = TestCase (assertEqual "Two named derivation register two vertices."
                              reg2
                              (registerDerivations list2))

test4 = TestCase (assertEqual "Five named derivation register five vertices."
                              reg3
                              (registerDerivations list3))

test5 = TestCase (assertEqual "Unnamed derivations are not added to graphs."
                              reg3
                              (registerDerivations list4))

-----------------------------------------------------------------------------------------
-- addDepToGraph

reg4 = fromJust $ addEdge reg3 rel2 rel1
reg5 = fromJust $ addEdge reg4 rel2 rel3

test6 = TestCase (assertEqual "Applying a known derivation is allowed (1/2)."
                              (Right reg4)
                              (addDepToGraph rel2 rwt1 reg3))

test7 = TestCase (assertEqual "Applying a known derivation is allowed (2/2)."
                              (Right reg5)
                              (addDepToGraph rel2 rwt3 reg4))

test8 = TestCase (assertEqual "Using a relation is a no-ops."
                              (Right reg5)
                              (addDepToGraph rel2 rwto reg5))

test9 = TestCase (assertEqual "Apply must be used on a known relation."
                  (Left (UnmetDep rel2 relx))
                  (addDepToGraph rel2 rwtx reg5))

-----------------------------------------------------------------------------------------
-- addDepsToGraph

test10 = TestCase (assertEqual "addDepsToGraph supports empty lists of rewrites"
                               (Right reg3)
                               (addDepsToGraph rel2 [] reg3))

test11 = TestCase (assertEqual "addDepsToGraph supports sinlgeton rewrite lists."
                               (Right reg4)
                               (addDepsToGraph rel2 [rwt1] reg3))

test12 = TestCase (assertEqual "addDepsToGraph supports rewrite pairs."
                               (Right reg5)
                               (addDepsToGraph rel2 [rwt1, rwt3] reg3))

test13 = TestCase (assertEqual "addDepsToGraph supports no-ops."
                               (Right reg5)
                               (addDepsToGraph rel2 rewrites reg3))
    where rewrites = [rwto, rwt1, rwto, rwt3, rwto]

test14 = TestCase (assertEqual "addDepsToGraph requires relations are known.."
                               (Left (UnmetDep rel2 relx))
                               (addDepsToGraph rel2 rewrites reg5))
    where rewrites = [rwto, rwt1, rwto, rwt3, rwto, rwtx]

-----------------------------------------------------------------------------------------
-- addDerivationToGraph

reg6 = fromJust $ addEdge reg5 rel3 rel1

test15 = TestCase (assertEqual "addDerivationToGraph supports unnamed derivations."
                               (Right reg3)
                               (addDerivationToGraph derivationo reg3))

test16 = TestCase (assertEqual "addDerivationToGraph rejects invalid unnamed derivations."
                               (Left (UnmetDep "" relx))
                               (addDerivationToGraph derivationx reg3))

test17 = TestCase (assertEqual "addDerivationToGraph supports dep-free derivations."
                               (Right reg3)
                               (addDerivationToGraph namedDerivation1 reg3))

test18 = TestCase (assertEqual "addDerivationToGraph supports valid derivations (1/2)."
                               (Right reg5)
                               (addDerivationToGraph namedDerivation2 reg3))

test19 = TestCase (assertEqual "addDerivationToGraph supports valid derivations (2/2)."
                               (Right reg6)
                               (addDerivationToGraph namedDerivation3 reg5))

test20 = TestCase (assertEqual "addDerivationToGraph rejects invalid derivations."
                               (Left (UnmetDep "rel1" relx))
                               (addDerivationToGraph namedDerivationx reg5))

-----------------------------------------------------------------------------------------
-- addDerivationsToGraph

reg7 = fromJust $ addEdge reg6 rel4 rel3
reg8 = fromJust $ addEdge reg7 rel5 rel3
reg9 = fromJust $ addEdge reg8 rel5 rel4
reg10 = fromJust $ addEdge reg9 "" rel1
reg11 = fromJust $ addEdge reg10 "" rel3

test21 = TestCase (assertEqual "Can add edges for derivations in list0."
                               (Right reg3)
                               (addDerivationsToGraph list0 reg3))

test22 = TestCase (assertEqual "Can add edges for derivations in list1."
                               (Right reg3)
                               (addDerivationsToGraph list1 reg3))

test23 = TestCase (assertEqual "Can add edges for derivations in list2."
                               (Right reg5)
                               (addDerivationsToGraph list2 reg3))

test24 = TestCase (assertEqual "Can add edges for derivations in list3."
                               (Right reg9)
                               (addDerivationsToGraph list3 reg3))

test25 = TestCase (assertEqual "Can add edges for derivations in list4."
                               (Right reg9)
                               (addDerivationsToGraph list4 reg3))

test26 = TestCase (assertEqual "Unnamed derivations are still recorded in the graph."
                               (Right reg11)
                               (addDerivationsToGraph list5 reg3))

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
                                     TestLabel "detectDerivationError_Cycle" test29]

main = defaultMain tests
