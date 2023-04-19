module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Edit.Internal.EIRules
import Lafont.Rewrite.Common
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Example rewrite rules.

sym1 = Symbol "x" []
sym2 = Symbol "y" []
sym3 = Symbol "z" []
sym4 = Symbol "w" []

w1 = [sym1, sym2, sym3, sym1]
w2 = [sym2, sym4, sym1, sym3]
w3 = [sym4]
w4 = [sym4, sym2]

nullrel = RewriteRule { lhs         = []
                      , rhs         = []
                      , equational  = True
                      , derivedFrom = Nothing
                      }

rel_lnull = RewriteRule { lhs         = []
                        , rhs         = w1
                        , equational  = False
                        , derivedFrom = Nothing
                        }

rel_rnull  = RewriteRule { lhs         = w2
                         , rhs         = []
                         , equational  = False
                         , derivedFrom = Nothing
                         }

rel_nonull = RewriteRule { lhs         = w3
                         , rhs         = w4
                         , equational  = False
                         , derivedFrom = Nothing
                         }

birel_rnull = RewriteRule { lhs         = w3
                          , rhs         = []
                          , equational  = True
                          , derivedFrom = Nothing
                          }

birel_lnull = RewriteRule { lhs         = []
                          , rhs         = w4
                          , equational  = True
                          , derivedFrom = Nothing
                          }

birel_nonull = RewriteRule { lhs         = w1
                           , rhs         = w4
                           , equational  = True
                           , derivedFrom = Nothing
                           }

birel_rnull_d = RewriteRule { lhs         = w3
                            , rhs         = []
                            , equational  = True
                            , derivedFrom = Just "src"
                            }

-----------------------------------------------------------------------------------------
-- toEDir

test1 = TestCase (assertEqual "toEDir rejects trivial relatins."
                              (Nothing :: Maybe (RuleDir, MonWord))
                              (toEDir nullrel))

test2 = TestCase (assertEqual "toEDir rejects directed introduction."
                              (Nothing :: Maybe (RuleDir, MonWord))
                              (toEDir rel_lnull))

test3 = TestCase (assertEqual "toEDir accepts directed elimination."
                              (Just (L2R, w2) :: Maybe (RuleDir, MonWord))
                              (toEDir rel_rnull))

test4 = TestCase (assertEqual "toEDir rejects relation without null."
                              (Nothing :: Maybe (RuleDir, MonWord))
                              (toEDir rel_nonull))

test5 = TestCase (assertEqual "toEDir accepts l2r bidirectional elimination."
                              (Just (L2R, w3) :: Maybe (RuleDir, MonWord))
                              (toEDir birel_rnull))

test6 = TestCase (assertEqual "toEDir accepts r2l bidirectional elimination."
                              (Just (R2L, w4) :: Maybe (RuleDir, MonWord))
                              (toEDir birel_lnull))

test7 = TestCase (assertEqual "toEDir rejects bidirectional relation without null."
                              (Nothing :: Maybe (RuleDir, MonWord))
                              (toEDir birel_nonull))

-----------------------------------------------------------------------------------------
-- toIDir

test8 = TestCase (assertEqual "toIDir rejects trivial relatins."
                              (Nothing :: Maybe (RuleDir, MonWord))
                              (toIDir nullrel))

test9 = TestCase (assertEqual "toIDir rejects directed introduction."
                              (Just (L2R, w1) :: Maybe (RuleDir, MonWord))
                              (toIDir rel_lnull))

test10 = TestCase (assertEqual "toIDir accepts directed elimination."
                               (Nothing :: Maybe (RuleDir, MonWord))
                               (toIDir rel_rnull))

test11 = TestCase (assertEqual "toIDir rejects relation without null."
                               (Nothing :: Maybe (RuleDir, MonWord))
                               (toIDir rel_nonull))

test12 = TestCase (assertEqual "toIDir accepts l2r bidirectional elimination."
                               (Just (R2L, w3) :: Maybe (RuleDir, MonWord))
                               (toIDir birel_rnull))

test13 = TestCase (assertEqual "toIDir accepts r2l bidirectional elimination."
                               (Just (L2R, w4) :: Maybe (RuleDir, MonWord))
                               (toIDir birel_lnull))

test14 = TestCase (assertEqual "toIDir rejects bidirectional relation without null."
                               (Nothing :: Maybe (RuleDir, MonWord))
                               (toIDir birel_nonull))

-----------------------------------------------------------------------------------------
-- asLeftDual

test15 = TestCase (assertEqual "asLeftDual handles empty strings (edge case)."
                               (Nothing :: Maybe (Symbol, MonWord))
                               (asLeftDual []))

test16 = TestCase (assertEqual "asLeftDual handles long string (1/2)."
                               (Just (sym1, dual) :: Maybe (Symbol, MonWord))
                               (asLeftDual w1))
    where dual = [sym1, sym2, sym3]

test17 = TestCase (assertEqual "asLeftDual handles long string (2/2)."
                               (Just (sym3, dual) :: Maybe (Symbol, MonWord))
                               (asLeftDual w2))
    where dual = [sym2, sym4, sym1]

test18 = TestCase (assertEqual "asLeftDual handles short strings."
                               (Just (sym4, []) :: Maybe (Symbol, MonWord))
                               (asLeftDual w3))

test19 = TestCase (assertEqual "asLeftDual handles pairings."
                               (Just (sym2, [sym4]) :: Maybe (Symbol, MonWord))
                               (asLeftDual w4))

-----------------------------------------------------------------------------------------
-- asRightDual

test20 = TestCase (assertEqual "asRightDual handles empty strings (edge case)."
                               (Nothing :: Maybe (Symbol, MonWord))
                               (asRightDual []))

test21 = TestCase (assertEqual "asRightDual handles long string (1/2)."
                               (Just (sym1, dual) :: Maybe (Symbol, MonWord))
                               (asRightDual w1))
    where dual = [sym2, sym3, sym1]

test22 = TestCase (assertEqual "asRightDual handles long string (2/2)."
                               (Just (sym2, dual) :: Maybe (Symbol, MonWord))
                               (asRightDual w2))
    where dual = [sym4, sym1, sym3]

test23 = TestCase (assertEqual "asLeftDual handles short strings."
                               (Just (sym4, []) :: Maybe (Symbol, MonWord))
                               (asRightDual w3))

test24 = TestCase (assertEqual "asLeftDual handles pairings."
                               (Just (sym4, [sym2]) :: Maybe (Symbol, MonWord))
                               (asRightDual w4))

-----------------------------------------------------------------------------------------
-- asERule

test25 = TestCase (assertEqual "asERule handles valid elimination with ldual (1/2)."
                               (Just (sym3, erule) :: Maybe (Symbol, EIRule))
                               (asERule True rname rel_rnull))
    where rname = "rel1"
          erule = EIRule rname [sym2, sym4, sym1] L2R False

test26 = TestCase (assertEqual "asERule handles valid elimination with ldual (2/2)."
                               (Just (sym2, erule) :: Maybe (Symbol, EIRule))
                               (asERule True rname birel_lnull))
    where rname = "rel2"
          erule = EIRule rname [sym4] R2L False

test27 = TestCase (assertEqual "asERule handles valid elimination with rdual (1/2)."
                               (Just (sym2, erule) :: Maybe (Symbol, EIRule))
                               (asERule False rname rel_rnull))
    where rname = "rel3"
          erule = EIRule rname [sym4, sym1, sym3] L2R False

test28 = TestCase (assertEqual "asERule handles valid elimination with rdual (2/2)."
                               (Just (sym4, erule) :: Maybe (Symbol, EIRule))
                               (asERule False rname birel_lnull))
    where rname = "rel3"
          erule = EIRule rname [sym2] R2L False

test29 = TestCase (assertEqual "asERule handles invalid elimination with ldual"
                               (Nothing :: Maybe (Symbol, EIRule))
                               (asERule True "x" rel_lnull))

test30 = TestCase (assertEqual "asERule handles invalid elimination with rdual"
                               (Nothing :: Maybe (Symbol, EIRule))
                               (asERule False "x" rel_lnull))

test31 = TestCase (assertEqual "asERule handles derived relations."
                               (Just (sym4, erule) :: Maybe (Symbol, EIRule))
                               (asERule True rname birel_rnull_d))
    where rname = "rel4"
          erule = EIRule rname [] L2R True

-----------------------------------------------------------------------------------------
-- asIRule

test32 = TestCase (assertEqual "asIRule handles valid elimination with ldual (1/2)."
                               (Just (sym1, erule) :: Maybe (Symbol, EIRule))
                               (asIRule True rname rel_lnull))
    where rname = "rel1"
          erule = EIRule rname [sym1, sym2, sym3] L2R False

test33 = TestCase (assertEqual "asIRule handles valid elimination with ldual (2/2)."
                               (Just (sym2, erule) :: Maybe (Symbol, EIRule))
                               (asIRule True rname birel_lnull))
    where rname = "rel2"
          erule = EIRule rname [sym4] L2R False

test34 = TestCase (assertEqual "asIRule handles valid elimination with rdual (1/2)."
                               (Just (sym1, erule) :: Maybe (Symbol, EIRule))
                               (asIRule False rname rel_lnull))
    where rname = "rel3"
          erule = EIRule rname [sym2, sym3, sym1] L2R False

test35 = TestCase (assertEqual "asIRule handles valid elimination with rdual (2/2)."
                               (Just (sym4, erule) :: Maybe (Symbol, EIRule))
                               (asIRule False rname birel_lnull))
    where rname = "rel3"
          erule = EIRule rname [sym2] L2R False

test36 = TestCase (assertEqual "asIRule handles invalid elimination with ldual"
                               (Nothing :: Maybe (Symbol, EIRule))
                               (asIRule True "x" rel_rnull))

test37 = TestCase (assertEqual "asIRule handles invalid elimination with rdual"
                               (Nothing :: Maybe (Symbol, EIRule))
                               (asIRule False "x" rel_rnull))

test38 = TestCase (assertEqual "asIRule handles derived relations."
                               (Just (sym4, erule) :: Maybe (Symbol, EIRule))
                               (asIRule True rname birel_rnull_d))
    where rname = "rel4"
          erule = EIRule rname [] R2L True

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "Internal_toEDir_triv" test1,
                                     TestLabel "Internal_toEDir_irel" test2,
                                     TestLabel "Internal_toEDir_erel" test3,
                                     TestLabel "Internal_toEDir_nonull" test4,
                                     TestLabel "Internal_toEDir_l2r_bidir" test5,
                                     TestLabel "Internal_toEDir_r2l_bidir" test6,
                                     TestLabel "Internal_toEDir_nonull_bidir" test7,
                                     TestLabel "Internal_toIDir_triv" test8,
                                     TestLabel "Internal_toIDir_irel" test9,
                                     TestLabel "Internal_toIDir_erel" test10,
                                     TestLabel "Internal_toIDir_nonull" test11,
                                     TestLabel "Internal_toIDir_l2r_bidir" test12,
                                     TestLabel "Internal_toIDir_r2l_bidir" test13,
                                     TestLabel "Internal_toIDir_nonull_bidir" test14,
                                     TestLabel "Internal_asLeftDual_epsilon" test15,
                                     TestLabel "Internal_asLeftDual_len4_a" test16,
                                     TestLabel "Internal_asLeftDual_len4_b" test17,
                                     TestLabel "Internal_asLeftDual_len1" test18,
                                     TestLabel "Internal_asLeftDual_len2" test19,
                                     TestLabel "Internal_asRightDual_epsilon" test20,
                                     TestLabel "Internal_asRightDual_len4_a" test21,
                                     TestLabel "Internal_asRightDual_len4_b" test22,
                                     TestLabel "Internal_asRightDual_len1" test23,
                                     TestLabel "Internal_asRightDual_len2" test24,
                                     TestLabel "Internal_asERule_Valid_LDual_1" test25,
                                     TestLabel "Internal_asERule_Valid_LDual_2" test26,
                                     TestLabel "Internal_asERule_Valid_RDual_1" test27,
                                     TestLabel "Internal_asERule_Valid_RDual_2" test28,
                                     TestLabel "Internal_asERule_Invalid_LDual" test29,
                                     TestLabel "Internal_asERule_Invalid_RDual" test30,
                                     TestLabel "Internal_asERule_Derived" test31,
                                     TestLabel "Internal_asIRule_Valid_LDual_1" test32,
                                     TestLabel "Internal_asIRule_Valid_LDual_2" test33,
                                     TestLabel "Internal_asIRule_Valid_RDual_1" test34,
                                     TestLabel "Internal_asIRule_Valid_RDual_2" test35,
                                     TestLabel "Internal_asIRule_Invalid_LDual" test36,
                                     TestLabel "Internal_asIRule_Invalid_RDual" test37,
                                     TestLabel "Internal_asIRule_Derived" test38]

main = defaultMain tests
