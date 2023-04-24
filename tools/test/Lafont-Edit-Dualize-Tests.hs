module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
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

selfdual1 = EIRule "sd1" [sym1] L2R True
selfdual2 = EIRule "sd2" [sym2] L2R False
selfdual3 = EIRule "sd3" [sym3] L2R True
selfdual4 = EIRule "sd4" [sym4] L2R False
selfdual5 = EIRule "sd5" [sym5] L2R True

selfdual_eview0 = createView
selfdual_eview1 = addRule selfdual_eview0 (sym4, selfdual4)
selfdual_eview2 = addRule selfdual_eview1 (sym5, selfdual5)

selfdual_iview0 = createView
selfdual_iview1 = addRule selfdual_iview0 (sym1, selfdual1)
selfdual_iview2 = addRule selfdual_iview1 (sym2, selfdual2)
selfdual_iview3 = addRule selfdual_iview2 (sym3, selfdual3)

alt1 = EIRule "alt1" [sym5, sym6] L2R True
alt2 = EIRule "alt2" [sym7, sym7, sym7] L2R True
alt3 = EIRule "alt3" [sym3] L2R True
alt4 = EIRule "alt4" [sym2, sym4] L2R True
alt5 = EIRule "alt5" [sym1] L2R True

alt_eview0 = createView
alt_eview1 = addRule alt_eview0 (sym4, alt4)
alt_eview2 = addRule alt_eview1 (sym5, alt5)

alt_iview0 = createView
alt_iview1 = addRule alt_iview0 (sym1, alt1)
alt_iview2 = addRule alt_iview1 (sym2, alt2)
alt_iview3 = addRule alt_iview2 (sym3, alt3)

-----------------------------------------------------------------------------------------
-- * dualizeRule

test1 = TestCase (assertEqual "dualizeRule fails when an elimination rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (dualizeRule selfdual_eview1 selfdual_iview3 rwrule1))

test2 = TestCase (assertEqual "dualizeRule fails when an introduction rule is missing."
                              (Nothing :: Maybe (MonWord, MonWord))
                              (dualizeRule selfdual_eview2 selfdual_iview2 rwrule1))

test3 = TestCase (assertEqual "dualizeRule applied to a self-dual case."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (dualizeRule selfdual_eview2 selfdual_iview3 rwrule1))
    where lhs = [sym4, sym5]
          rhs = [sym3, sym2, sym1]

-- This case was also validated by-hand.
test4 = TestCase (assertEqual "dualizeRule applied to a non-trivial rules."
                              (Just (lhs, rhs) :: Maybe (MonWord, MonWord))
                              (dualizeRule alt_eview2 alt_iview3 rwrule1))
    where lhs = [sym2, sym4, sym1]
          rhs = [sym3, sym7, sym7, sym7, sym5, sym6]

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "dualizeRule_Missing_ERule" test1,
                                     TestLabel "dualizeRule_Missing_IRule" test2,
                                     TestLabel "dualizeRule_SelfDual" test3,
                                     TestLabel "dualizeRule_Complex" test4]

main = defaultMain tests
