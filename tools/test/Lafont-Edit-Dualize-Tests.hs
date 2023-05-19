module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Lafont.Common
import Lafont.Edit.Dualize
import Lafont.Edit.EIRules
import Lafont.Edit.Internal.EIRules
import Lafont.Rewrite.Common
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Test structures.

sym1 = Symbol "x" []
sym2 = Symbol "y" []
sym3 = Symbol "z" []
sym4 = Symbol "w" []
sym5 = Symbol "a" []
sym6 = Symbol "b" []
sym7 = Symbol "c" []

rwrule1 = RewriteRule { lhs         = [sym1, sym2, sym3]
                      , rhs         = [sym5, sym4]
                      , equational  = False
                      , derivedFrom = Nothing
                      }

mkSelfDualEView :: IsLeftDual -> (EIView, EIView)
mkSelfDualEView isLeftDual = (eview1, eview2)
    where erule1 = EIRule "sd4" [sym4] L2R isLeftDual False
          erule2 = EIRule "sd5" [sym5] L2R isLeftDual True
          eview0 = createView isLeftDual
          eview1 = fromJust $ addRule eview0 (sym4, erule1)
          eview2 = fromJust $ addRule eview1 (sym5, erule2)

mkSelfDualIView :: IsLeftDual -> (EIView, EIView)
mkSelfDualIView isLeftDual = (eview2, eview3)
    where erule1 = EIRule "sd1" [sym1] L2R isLeftDual True
          erule2 = EIRule "sd2" [sym2] L2R isLeftDual False
          erule3 = EIRule "sd3" [sym3] L2R isLeftDual True
          eview0 = createView isLeftDual
          eview1 = fromJust $ addRule eview0 (sym1, erule1)
          eview2 = fromJust $ addRule eview1 (sym2, erule2)
          eview3 = fromJust $ addRule eview2 (sym3, erule3)

(rsd_eview1, rsd_eview2) = mkSelfDualEView False
(lsd_iview1, lsd_iview2) = mkSelfDualIView True

getAltRule :: Int -> IsLeftDual -> EIRule
getAltRule 1 x = EIRule "alt1" [sym5, sym6]       L2R x True
getAltRule 2 x = EIRule "alt2" [sym7, sym7, sym7] L2R x True
getAltRule 3 x = EIRule "alt3" [sym3]             L2R x True
getAltRule 4 x = EIRule "alt4" [sym2, sym4]       L2R x True
getAltRule 5 x = EIRule "alt5" [sym1]             L2R x True
getAltRule _ _ = error "Unknown alt rule."

mkAltEView :: IsLeftDual -> EIView
mkAltEView isLeftDual = eview2
    where eview0 = createView isLeftDual
          eview1 = fromJust $ addRule eview0 (sym4, getAltRule 4 isLeftDual)
          eview2 = fromJust $ addRule eview1 (sym5, getAltRule 5 isLeftDual)

mkAltIView :: IsLeftDual -> EIView
mkAltIView isLeftDual = iview3
    where iview0 = createView isLeftDual
          iview1 = fromJust $ addRule iview0 (sym1, getAltRule 1 isLeftDual)
          iview2 = fromJust $ addRule iview1 (sym2, getAltRule 2 isLeftDual)
          iview3 = fromJust $ addRule iview2 (sym3, getAltRule 3 isLeftDual)

lalt_eview = mkAltEView True
lalt_iview = mkAltIView True
ralt_iview = mkAltIView False

-----------------------------------------------------------------------------------------
-- * dualizeRule

test1 = TestCase (assertEqual "dualizeRule fails when an elimination rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (dualizeRule rsd_eview1 lsd_iview2 rwrule1))

test2 = TestCase (assertEqual "dualizeRule fails when an introduction rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (dualizeRule rsd_eview2 lsd_iview1 rwrule1))

test3 = TestCase (assertEqual "dualizeRule applied to a self-dual case."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (dualizeRule rsd_eview2 lsd_iview2 rwrule1))
    where lhs = [sym4, sym5]
          rhs = [sym3, sym2, sym1]

-- This case was also validated by-hand.
test4 = TestCase (assertEqual "dualizeRule applied to a non-trivial rules."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (dualizeRule lalt_eview ralt_iview rwrule1))
    where lhs = [sym2, sym4, sym1]
          rhs = [sym3, sym7, sym7, sym7, sym5, sym6]

-----------------------------------------------------------------------------------------
-- * deriveIntro

test5 = TestCase (assertEqual "deriveIntro applied to an empty word."
                              (Just (5, []) :: Maybe (Int, [EIRewrite]))
                              (deriveIntro lalt_iview 5 []))

test6 = TestCase (assertEqual "deriveIntro applied to a non-matching word."
                              (Nothing :: Maybe (Int, [EIRewrite]))
                              (deriveIntro lalt_iview 5 [sym1, sym2, sym5]))

test7 = TestCase (assertEqual "deriveIntro applied with left duals."
                              (Just (11, deriv) :: Maybe (Int, [EIRewrite]))
                              (deriveIntro lalt_iview 5 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 True
          r2    = getAltRule 2 True
          r3    = getAltRule 3 True
          deriv = [EIRewrite 5 r3, EIRewrite 6 r2, EIRewrite 9 r1]

test8 = TestCase (assertEqual "deriveIntro applied with right duals."
                              (Just (8, deriv) :: Maybe (Int, [EIRewrite]))
                              (deriveIntro ralt_iview 5 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 False
          r2    = getAltRule 2 False
          r3    = getAltRule 3 False
          deriv = [EIRewrite 5 r1, EIRewrite 6 r2, EIRewrite 7 r3]

-----------------------------------------------------------------------------------------
-- * deriveElim

test9 = TestCase (assertEqual "deriveElim applied to an empty word."
                              (Just (5, []) :: Maybe (Int, [EIRewrite]))
                              (deriveElim lalt_iview 5 []))

test10 = TestCase (assertEqual "deriveElim applied to a non-matching word."
                               (Nothing :: Maybe (Int, [EIRewrite]))
                               (deriveElim lalt_iview 5 [sym1, sym2, sym5]))

test11 = TestCase (assertEqual "deriveElim applied with left duals."
                               (Just (4, deriv) :: Maybe (Int, [EIRewrite]))
                               (deriveElim lalt_iview 10 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 True
          r2    = getAltRule 2 True
          r3    = getAltRule 3 True
          deriv = [EIRewrite 10 r1, EIRewrite 8 r2, EIRewrite 5 r3]

test12 = TestCase (assertEqual "deriveElim applied with right duals."
                               (Just (7, deriv) :: Maybe (Int, [EIRewrite]))
                               (deriveElim ralt_iview 10 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 False
          r2    = getAltRule 2 False
          r3    = getAltRule 3 False
          deriv = [EIRewrite 10 r3, EIRewrite 9 r2, EIRewrite 8 r1]

-----------------------------------------------------------------------------------------
-- * Left vs. Right Duals

test13 = TestCase (assertBool "hasLeftDuals works for left duals (1/2)."
                              (hasLeftDuals lalt_eview))

test14 = TestCase (assertBool "hasLeftDuals works for left duals (2/2)."
                              (hasLeftDuals lsd_iview2))

test15 = TestCase (assertBool "hasLeftDuals works for right duals (1/2)."
                              (not $ hasLeftDuals ralt_iview))

test16 = TestCase (assertBool "hasLeftDuals works for right duals (2/2)."
                              (not $ hasLeftDuals rsd_eview2))

test17 = TestCase (assertEqual "addRule ensures left-handedness."
                               (Nothing :: Maybe EIView)
                               (addRule lsd_iview2 (sym1, getAltRule 2 False)))

test18 = TestCase (assertEqual "addRule ensures right-handedness."
                               (Nothing :: Maybe EIView)
                               (addRule rsd_eview2 (sym1, getAltRule 2 True)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "dualizeRule_Missing_ERule" test1,
                                     TestLabel "dualizeRule_Missing_IRule" test2,
                                     TestLabel "dualizeRule_SelfDual" test3,
                                     TestLabel "dualizeRule_Complex" test4,
                                     TestLabel "deriveIntro_Empty" test5,
                                     TestLabel "deriveIntro_NoMatch" test6,
                                     TestLabel "deriveIntro_LeftDual" test7,
                                     TestLabel "deriveIntro_RightDual" test8,
                                     TestLabel "deriveElim_Empty" test9,
                                     TestLabel "deriveElim_NoMatch" test10,
                                     TestLabel "deriveElim_LeftDual" test11,
                                     TestLabel "deriveElim_RightDual" test12,
                                     TestLabel "hasLeftDuals_Left_1" test13,
                                     TestLabel "hasLeftDuals_Left_2" test14,
                                     TestLabel "hasLeftDuals_Right_1" test15,
                                     TestLabel "hasLeftDuals_Right_2" test16,
                                     TestLabel "addRule_Lefthand" test17,
                                     TestLabel "addRule_Righthand" test18]

main = defaultMain tests
