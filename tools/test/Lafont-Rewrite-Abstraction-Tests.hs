module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Abstraction
import Lafont.Rewrite.Common
import Lafont.Rewrite.Rules
import Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- Declares derivations for tests.

rel1 = "rel1"
rel2 = "rel2"
rel3 = "rel3"
relx = "relx"

summary1 = DerivationSummary (RewritePreamble (Just rel1)) [] []
summary2 = DerivationSummary (RewritePreamble (Just rel2)) [] []
summary3 = DerivationSummary (RewritePreamble (Just rel3)) [] []

symb1 = Symbol "S" []
symb2 = Symbol "T" []
symb3 = Symbol "U" []

word1 = [symb1, symb1, symb1]
word2 = [symb2, symb2, symb2]
word3 = [symb3, symb3, symb3]

rule1 = RewriteRule word1 word2 True Nothing
rule2 = RewriteRule word1 word3 True Nothing
rule3 = RewriteRule word2 word3 True Nothing

rw1 = Rewrite rule1 0 L2R
rw2 = Rewrite rule2 0 L2R
rw3 = Rewrite rule3 0 L2R

rewrites1 :: [AbsRewrite]
rewrites1 = [Left rw1, Left rw1, Left rw1]

rewrites2 :: [AbsRewrite]
rewrites2 = [Left rw2, Left rw2, Left rw2]

rewrites3 :: [AbsRewrite]
rewrites3 = [Left rw3, Left rw3, Left rw3]

derivation1 = AbsDerivation summary1 rewrites1
derivation2 = AbsDerivation summary2 rewrites2
derivation3 = AbsDerivation summary3 rewrites3

-----------------------------------------------------------------------------------------
-- Declares derivations for tests.

map0 = makeDerivationMap []
map1 = makeDerivationMap [derivation1]
map2 = makeDerivationMap [derivation1, derivation2]
map3 = makeDerivationMap [derivation1, derivation2, derivation3]

test1 = TestCase $ (assertEqual "makeDerivationMap after 0 inserts (1/4)."
                                Nothing
                                (getDerivation map0 rel1))

test2 = TestCase $ (assertEqual "makeDerivationMap after 0 inserts (2/4)."
                                Nothing
                                (getDerivation map0 rel2))

test3 = TestCase $ (assertEqual "makeDerivationMap after 0 inserts (3/4)."
                                Nothing
                                (getDerivation map0 rel3))

test4 = TestCase $ (assertEqual "makeDerivationMap after 0 inserts (4/4)."
                                Nothing
                                (getDerivation map0 relx))

test5 = TestCase $ (assertEqual "makeDerivationMap after 1 insert (1/4)."
                                (Just derivation1)
                                (getDerivation map1 rel1))

test6 = TestCase $ (assertEqual "makeDerivationMap after 1 insert (2/4)."
                                Nothing
                                (getDerivation map1 rel2))

test7 = TestCase $ (assertEqual "makeDerivationMap after 1 insert (3/4)."
                                Nothing
                                (getDerivation map1 rel3))

test8 = TestCase $ (assertEqual "makeDerivationMap after 1 insert (4/4)."
                                Nothing
                                (getDerivation map1 relx))

test9 = TestCase $ (assertEqual "makeDerivationMap after 2 inserts (1/4)."
                                (Just derivation1)
                                (getDerivation map2 rel1))

test10 = TestCase $ (assertEqual "makeDerivationMap after 2 inserts (2/4)."
                                 (Just derivation2)
                                 (getDerivation map2 rel2))

test11 = TestCase $ (assertEqual "makeDerivationMap after 2 inserts (3/4)."
                                 Nothing
                                 (getDerivation map2 rel3))

test12 = TestCase $ (assertEqual "makeDerivationMap after 2 inserts (4/4)."
                                 Nothing
                                 (getDerivation map2 relx))

test13 = TestCase $ (assertEqual "makeDerivationMap after 3 inserts (1/4)."
                                 (Just derivation1)
                                 (getDerivation map3 rel1))

test14 = TestCase $ (assertEqual "makeDerivationMap after 3 inserts (2/4)."
                                 (Just derivation2)
                                 (getDerivation map3 rel2))

test15 = TestCase $ (assertEqual "makeDerivationMap after 3 inserts (3/4)."
                                 (Just derivation3)
                                 (getDerivation map3 rel3))

test16 = TestCase $ (assertEqual "makeDerivationMap after 3 inserts (4/4)."
                                 Nothing
                                 (getDerivation map3 relx))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "makeDerivationMap_0Insert_1" test1,
                                     TestLabel "makeDerivationMap_0Insert_2" test2,
                                     TestLabel "makeDerivationMap_0Insert_3" test3,
                                     TestLabel "makeDerivationMap_0Insert_4" test4,
                                     TestLabel "makeDerivationMap_1Insert_1" test5,
                                     TestLabel "makeDerivationMap_1Insert_2" test6,
                                     TestLabel "makeDerivationMap_1Insert_3" test7,
                                     TestLabel "makeDerivationMap_1Insert_4" test8,
                                     TestLabel "makeDerivationMap_2Insert_1" test9,
                                     TestLabel "makeDerivationMap_2Insert_2" test10,
                                     TestLabel "makeDerivationMap_2Insert_3" test11,
                                     TestLabel "makeDerivationMap_2Insert_4" test12,
                                     TestLabel "makeDerivationMap_3Insert_1" test13,
                                     TestLabel "makeDerivationMap_3Insert_2" test14,
                                     TestLabel "makeDerivationMap_3Insert_3" test15,
                                     TestLabel "makeDerivationMap_3Insert_4" test16]

main = defaultMain tests
