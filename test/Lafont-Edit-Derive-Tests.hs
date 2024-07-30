module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Common
import Tietze.Rewrite.Common
import Tietze.Rewrite.Rules
import Tietze.Rewrite.Lookup
import Tietze.Edit.Derive

import qualified Data.Set as Set

-----------------------------------------------------------------------------------------
-- Rule Dictionary Preparation.

word1a :: MonWord
word1a = [(Symbol "a" [])]

word1b :: MonWord
word1b = [(Symbol "b" [])]

rel1 :: (String, RewriteRule)
rel1 = ("r1", (RewriteRule word1a word1b True (Primitive "r1")))

word2a :: MonWord
word2a = [(Symbol "b" []), (Symbol "b" []), (Symbol "b" [])]

word2b :: MonWord
word2b = [(Symbol "c" [])]

rel2 :: (String, RewriteRule)
rel2 = ("r2", (RewriteRule word2a word2b False (Primitive "r2")))

word3a :: MonWord
word3a = [(Symbol "a" []), (Symbol "a" []), (Symbol "a" [])]

word3b :: MonWord
word3b = [(Symbol "c" [])]

rel3 :: (String, RewriteRule)
rel3 = ("r3", (RewriteRule word3a word3b False (Primitive "r3")))

testDict0 :: RuleDict
testDict0 = empty
testDict1 = addRule testDict0 rel1
testDict2 = addRule testDict1 rel2
testDict3 = addRule testDict2 rel3

-----------------------------------------------------------------------------------------
-- rulesToOptions Tests.

exclusion1 = Set.fromList ["r3"]
exclusion2 = Set.fromList ["r1", "r2", "r3"]

fullOpts = rulesToOptions testDict3 Set.empty
stdOpts  = rulesToOptions testDict3 exclusion1

test1 = TestCase (assertEqual "Can extract rules from an empty dictionary."
                              0
                              (length $ rulesToOptions testDict0 Set.empty))

test2 = TestCase (assertEqual "Can extract rules from an empty dictionary with exclusion."
                              0
                              (length $ rulesToOptions testDict0 exclusion1))

test3 = TestCase (assertEqual "Can extract rules from a full dictionary."
                              4
                              (length fullOpts))

test4 = TestCase (assertEqual "Can extract rules from a full dictionary with exclusion."
                              3
                              (length stdOpts))

test5 = TestCase (assertEqual "Can extract rules from a full dictionary, full exclusion."
                              0
                              (length $ rulesToOptions testDict3 exclusion2))

-----------------------------------------------------------------------------------------
-- Constructs Logs.

nocache    = newDeriveLog 0
fullCache  = newDeriveLog (-1)
smCache    = newDeriveLog 3
largeCache = newDeriveLog 1000

isFailure :: Either DeriveLog [(String, Rewrite)] -> Bool
isFailure (Left _) = True
isFailure _        = False

-----------------------------------------------------------------------------------------
-- deriveRule: Deriving c from aaa.

sol1 = [("r1", Rewrite (snd rel1) 0 L2R),
        ("r1", Rewrite (snd rel1) 1 L2R),
        ("r1", Rewrite (snd rel1) 2 L2R),
        ("r2", Rewrite (snd rel2) 0 L2R)]

test6 = TestCase (assertEqual "Can derive c from aaa without caching (exact steps)."
                              (Right sol1)
                              (deriveRule nocache stdOpts word3b word3a 4))

test7 = TestCase (assertEqual "Can derive c from aaa without caching (extra steps)."
                              (Right sol1)
                              (deriveRule nocache stdOpts word3b word3a 5))

test8 = TestCase (assertEqual "Can derive c from aaa with caching (exact steps)."
                              (Right sol1)
                              (deriveRule fullCache stdOpts word3b word3a 4))

test9 = TestCase (assertEqual "Can derive c from aaa with caching (extra steps)."
                              (Right sol1)
                              (deriveRule fullCache stdOpts word3b word3a 5))

test10 = TestCase (assertEqual "Can derive c from aaa with fixed caching (exact steps)."
                               (Right sol1)
                               (deriveRule smCache stdOpts word3b word3a 4))

test11 = TestCase (assertEqual "Can derive c from aaa with fixed caching (extra steps)."
                               (Right sol1)
                               (deriveRule smCache stdOpts word3b word3a 5))

test12 = TestCase (assertEqual "Can derive c from aaa via an alternative proof."
                               (Right [("r3", Rewrite (snd rel3) 0 L2R)])
                               (deriveRule nocache fullOpts word3b word3a 1))

test13 = TestCase (assertEqual "Cannot derive c from aaa with too few steps."
                               (Left nocache)
                               (deriveRule nocache stdOpts word3b word3a 3))

-----------------------------------------------------------------------------------------
-- deriveRule: Deriving aaa from c.

test14 = TestCase (assertEqual "Cannot derive aaa from c without caching."
                               (Left nocache)
                               (deriveRule nocache stdOpts word3a word3b 4))

test15 = TestCase (assertBool "Cannot derive aaa from c with caching."
                              (isFailure $ deriveRule fullCache stdOpts word3a word3b 4))

test16 = TestCase (assertBool "Cannot derive aaa from c with fixed caching."
                              (isFailure $ deriveRule smCache stdOpts word3a word3b 4))

-----------------------------------------------------------------------------------------
-- deriveRule: Deriving aaa from bbb.

sol2 = [("r1", Rewrite (snd rel1) 0 R2L),
        ("r1", Rewrite (snd rel1) 1 R2L),
        ("r1", Rewrite (snd rel1) 2 R2L)]

test17 = TestCase (assertEqual "Can derive aaa from bbb with caching (exact steps)."
                               (Right sol2)
                               (deriveRule nocache stdOpts word3a word2a 3))

test18 = TestCase (assertEqual "Can derive aaa from bbb with caching (extra steps)."
                               (Right sol2)
                               (deriveRule nocache stdOpts word3a word2a 3))

test19 = TestCase (assertEqual "Cannot derive aaa from bbb with too few steps."
                               (Left nocache)
                               (deriveRule nocache stdOpts word3a word2a 2))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "rulesToOptions_Test1" test1,
                                     TestLabel "rulesToOptions_Test2" test2,
                                     TestLabel "rulesToOptions_Test3" test3,
                                     TestLabel "rulesToOptions_Test4" test4,
                                     TestLabel "rulesToOptions_Test5" test5,
                                     TestLabel "deriveRule_Case1_Conf1" test6,
                                     TestLabel "deriveRule_Case1_Conf2" test7,
                                     TestLabel "deriveRule_Case1_Conf3" test8,
                                     TestLabel "deriveRule_Case1_Conf4" test9,
                                     TestLabel "deriveRule_Case1_Conf5" test10,
                                     TestLabel "deriveRule_Case1_Conf6" test11,
                                     TestLabel "deriveRule_Case1_Alt" test12,
                                     TestLabel "deriveRule_Case1_Bnd" test13,
                                     TestLabel "deriveRule_Case2_Conf1" test14,
                                     TestLabel "deriveRule_Case2_Conf2" test15,
                                     TestLabel "deriveRule_Case2_Conf3" test16,
                                     TestLabel "deriveRule_Case3_Conf1" test17,
                                     TestLabel "deriveRule_Case3_Conf2" test18,
                                     TestLabel "deriveRule_Case3_Conf3" test19]

main = defaultMain tests