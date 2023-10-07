module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Data.Maybe
import Lafont.Common
import Lafont.Edit.Invert
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

mkSelfInvEView :: IsLeftInv -> (EIView, EIView)
mkSelfInvEView isLeftInv = (eview1, eview2)
    where erule1 = EIRule "sd4" [sym4] L2R isLeftInv False
          erule2 = EIRule "sd5" [sym5] L2R isLeftInv True
          eview0 = createView isLeftInv
          eview1 = fromJust $ addRule eview0 (sym4, erule1)
          eview2 = fromJust $ addRule eview1 (sym5, erule2)

mkSelfInvIView :: IsLeftInv -> (EIView, EIView)
mkSelfInvIView isLeftInv = (eview2, eview3)
    where erule1 = EIRule "sd1" [sym1] L2R isLeftInv True
          erule2 = EIRule "sd2" [sym2] L2R isLeftInv False
          erule3 = EIRule "sd3" [sym3] L2R isLeftInv True
          eview0 = createView isLeftInv
          eview1 = fromJust $ addRule eview0 (sym1, erule1)
          eview2 = fromJust $ addRule eview1 (sym2, erule2)
          eview3 = fromJust $ addRule eview2 (sym3, erule3)

(rsd_eview1, rsd_eview2) = mkSelfInvEView False
(lsd_iview1, lsd_iview2) = mkSelfInvIView True

getAltRule :: Int -> IsLeftInv -> EIRule
getAltRule 1 x = EIRule "alt1" [sym5, sym6]       L2R x True
getAltRule 2 x = EIRule "alt2" [sym7, sym7, sym7] L2R x True
getAltRule 3 x = EIRule "alt3" [sym3]             L2R x True
getAltRule 4 x = EIRule "alt4" [sym2, sym4]       L2R x True
getAltRule 5 x = EIRule "alt5" [sym1]             L2R x True
getAltRule _ _ = error "Unknown alt rule."

mkAltEView :: IsLeftInv -> EIView
mkAltEView isLeftInv = eview2
    where eview0 = createView isLeftInv
          eview1 = fromJust $ addRule eview0 (sym4, getAltRule 4 isLeftInv)
          eview2 = fromJust $ addRule eview1 (sym5, getAltRule 5 isLeftInv)

mkAltIView :: IsLeftInv -> EIView
mkAltIView isLeftInv = iview3
    where iview0 = createView isLeftInv
          iview1 = fromJust $ addRule iview0 (sym1, getAltRule 1 isLeftInv)
          iview2 = fromJust $ addRule iview1 (sym2, getAltRule 2 isLeftInv)
          iview3 = fromJust $ addRule iview2 (sym3, getAltRule 3 isLeftInv)

lalt_eview = mkAltEView True
lalt_iview = mkAltIView True
ralt_iview = mkAltIView False

-----------------------------------------------------------------------------------------
-- * invertRule

test1 = TestCase (assertEqual "invertRule fails when an elimination rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (invertRule rsd_eview1 lsd_iview2 rwrule1))

test2 = TestCase (assertEqual "invertRule fails when an introduction rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (invertRule rsd_eview2 lsd_iview1 rwrule1))

test3 = TestCase (assertEqual "invertRule applied to a self-inv case."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (invertRule rsd_eview2 lsd_iview2 rwrule1))
    where lhs = [sym4, sym5]
          rhs = [sym3, sym2, sym1]

-- This case was also validated by-hand.
test4 = TestCase (assertEqual "invertRule applied to a non-trivial rules."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (invertRule lalt_eview ralt_iview rwrule1))
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

test7 = TestCase (assertEqual "deriveIntro applied with left invs."
                              (Just (11, deriv) :: Maybe (Int, [EIRewrite]))
                              (deriveIntro lalt_iview 5 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 True
          r2    = getAltRule 2 True
          r3    = getAltRule 3 True
          deriv = [EIRewrite 5 r3, EIRewrite 6 r2, EIRewrite 9 r1]

test8 = TestCase (assertEqual "deriveIntro applied with right invs."
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

test11 = TestCase (assertEqual "deriveElim applied with left invs."
                               (Just (4, deriv) :: Maybe (Int, [EIRewrite]))
                               (deriveElim lalt_iview 10 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 True
          r2    = getAltRule 2 True
          r3    = getAltRule 3 True
          deriv = [EIRewrite 10 r1, EIRewrite 8 r2, EIRewrite 5 r3]

test12 = TestCase (assertEqual "deriveElim applied with right invs."
                               (Just (7, deriv) :: Maybe (Int, [EIRewrite]))
                               (deriveElim ralt_iview 10 [sym1, sym2, sym3]))
    where r1    = getAltRule 1 False
          r2    = getAltRule 2 False
          r3    = getAltRule 3 False
          deriv = [EIRewrite 10 r3, EIRewrite 9 r2, EIRewrite 8 r1]

-----------------------------------------------------------------------------------------
-- * Left vs. Right Invs

test13 = TestCase (assertBool "hasLeftInvs works for left invs (1/2)."
                              (hasLeftInvs lalt_eview))

test14 = TestCase (assertBool "hasLeftInvs works for left invs (2/2)."
                              (hasLeftInvs lsd_iview2))

test15 = TestCase (assertBool "hasLeftInvs works for right invs (1/2)."
                              (not $ hasLeftInvs ralt_iview))

test16 = TestCase (assertBool "hasLeftInvs works for right invs (2/2)."
                              (not $ hasLeftInvs rsd_eview2))

test17 = TestCase (assertEqual "addRule ensures left-handedness."
                               (Nothing :: Maybe EIView)
                               (addRule lsd_iview2 (sym1, getAltRule 2 False)))

test18 = TestCase (assertEqual "addRule ensures right-handedness."
                               (Nothing :: Maybe EIView)
                               (addRule rsd_eview2 (sym1, getAltRule 2 True)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "invertRule_Missing_ERule" test1,
                                     TestLabel "invertRule_Missing_IRule" test2,
                                     TestLabel "invertRule_SelfInv" test3,
                                     TestLabel "invertRule_Complex" test4,
                                     TestLabel "deriveIntro_Empty" test5,
                                     TestLabel "deriveIntro_NoMatch" test6,
                                     TestLabel "deriveIntro_LeftInv" test7,
                                     TestLabel "deriveIntro_RightInv" test8,
                                     TestLabel "deriveElim_Empty" test9,
                                     TestLabel "deriveElim_NoMatch" test10,
                                     TestLabel "deriveElim_LeftInv" test11,
                                     TestLabel "deriveElim_RightInv" test12,
                                     TestLabel "hasLeftInvs_Left_1" test13,
                                     TestLabel "hasLeftInvs_Left_2" test14,
                                     TestLabel "hasLeftInvs_Right_1" test15,
                                     TestLabel "hasLeftInvs_Right_2" test16,
                                     TestLabel "addRule_Lefthand" test17,
                                     TestLabel "addRule_Righthand" test18]

main = defaultMain tests
