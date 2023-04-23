module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Edit.EIRules
import Lafont.Edit.Internal.EIRules
import Lafont.Rewrite.Common
import Lafont.Rewrite.Lookup
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

rel_rnull = RewriteRule { lhs         = w2
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
-- toEDict

-- Intended Test:
-- 1. elookup0: an empty lookup.
-- 2. elookup3: a populated lookup without any elimination rules.
-- 3. elookup7: a populated lookup with matches.
elookup0 = empty
elookup1 = addRule elookup0 ("r1", birel_nonull)
elookup2 = addRule elookup1 ("r2", rel_nonull)
elookup3 = addRule elookup2 ("r3", rel_lnull)
elookup4 = addRule elookup3 ("r4", rel_rnull)
elookup5 = addRule elookup4 ("r5", birel_nonull)
elookup6 = addRule elookup5 ("r6", birel_rnull)
elookup7 = addRule elookup6 ("r7", birel_lnull)
elookup8 = addRule elookup7 ("r8", birel_rnull_d)

-- Left duals.

test39 = TestCase (assertEqual "toEDict handles empty dictionaries (left duals)."
                               0
                               (getEICount $ toEDict True elookup0))

test40 = TestCase (assertEqual "toEDict handles non-matching dictionaries (left duals)."
                               0
                               (getEICount $ toEDict True elookup3))

test41 = TestCase (assertEqual "toEDict handles matches (left dual, size)."
                               4
                               (getEICount $ toEDict True elookup8))

test42 = TestCase (assertEqual "toEDict handles matches (left dual, @sym3)."
                               [erule]
                               (getEIRule (toEDict True elookup8) sym3))
    where erule = EIRule "r4" [sym2, sym4, sym1] L2R False

test43 = TestCase (assertEqual "toEDict handles matches (left dual, @sym4)."
                               [erule1, erule2]
                               (getEIRule (toEDict True elookup8) sym4))
    where erule1 = EIRule "r6" [] L2R False
          erule2 = EIRule "r8" [] L2R True

test44 = TestCase (assertEqual "toEDict handles matches (left dual, @sym2)."
                               [erule]
                               (getEIRule (toEDict True elookup8) sym2))
    where erule = EIRule "r7" [sym4] R2L False

-- Right duals.

test45 = TestCase (assertEqual "toEDict handles empty dictionaries (right duals)."
                               0
                               (getEICount $ toEDict False elookup0))

test46 = TestCase (assertEqual "toEDict handles non-matching dictionaries (right duals)."
                               0
                               (getEICount $ toEDict False elookup3))

test47 = TestCase (assertEqual "toEDict handles matches (right dual, size)."
                               4
                               (getEICount $ toEDict False elookup8))

test48 = TestCase (assertEqual "toEDict handles matches (right dual, @sym2)."
                               [erule]
                               (getEIRule (toEDict False elookup8) sym2))
    where erule = EIRule "r4" [sym4, sym1, sym3] L2R False

test49 = TestCase (assertEqual "toEDict handles matches (right dual, @sym4)."
                               [erule1, erule2, erule3]
                               (getEIRule (toEDict False elookup8) sym4))
    where erule1 = EIRule "r6" [] L2R False
          erule2 = EIRule "r7" [sym2] R2L False
          erule3 = EIRule "r8" [] L2R True

-----------------------------------------------------------------------------------------
-- toIDict

-- Intended Test:
-- 1. elookup0: an empty lookup.
-- 2. elookup3: a populated lookup without any elimination rules.
-- 3. elookup7: a populated lookup with matches.
ilookup0 = empty
ilookup1 = addRule ilookup0 ("rel1", birel_nonull)
ilookup2 = addRule ilookup1 ("rel2", rel_nonull)
ilookup3 = addRule ilookup2 ("rel3", rel_rnull)
ilookup4 = addRule ilookup3 ("rel4", rel_lnull)
ilookup5 = addRule ilookup4 ("rel5", birel_nonull)
ilookup6 = addRule ilookup5 ("rel6", birel_rnull)
ilookup7 = addRule ilookup6 ("rel7", birel_lnull)
ilookup8 = addRule ilookup7 ("rel8", birel_rnull_d)

-- Left duals.

test50 = TestCase (assertEqual "toIDict handles empty dictionaries (left duals)."
                               0
                               (getEICount $ toIDict True ilookup0))

test51 = TestCase (assertEqual "toIDict handles non-matching dictionaries (left duals)."
                               0
                               (getEICount $ toIDict True ilookup3))

test52 = TestCase (assertEqual "toIDict handles matches (left dual, @sym1)."
                               [erule]
                               (getEIRule (toIDict True ilookup8) sym1))
    where erule = EIRule "rel4" [sym1, sym2, sym3] L2R False

test53 = TestCase (assertEqual "toIDict handles matches (left dual, @sym4)."
                               [erule1, erule2]
                               (getEIRule (toIDict True ilookup8) sym4))
    where erule1 = EIRule "rel6" [] R2L False
          erule2 = EIRule "rel8" [] R2L True

test54 = TestCase (assertEqual "toIDict handles matches (left dual, @sym2)."
                               [erule]
                               (getEIRule (toIDict True ilookup8) sym2))
    where erule = EIRule "rel7" [sym4] L2R False

-- Right duals.

test55 = TestCase (assertEqual "toIDict handles empty dictionaries (right duals)."
                               0
                               (getEICount $ toIDict False ilookup0))

test56 = TestCase (assertEqual "toIDict handles non-matching dictionaries (right duals)."
                               0
                               (getEICount $ toIDict False ilookup3))

test57 = TestCase (assertEqual "toIDict handles matches (right dual, @sym1)."
                               [erule]
                               (getEIRule (toIDict False ilookup8) sym1))
    where erule = EIRule "rel4" [sym2, sym3, sym1] L2R False

test58 = TestCase (assertEqual "toIDict handles matches (right dual, @sym4)."
                               [erule1, erule2, erule3]
                               (getEIRule (toIDict False elookup8) sym4))
    where erule1 = EIRule "r6" [] R2L False
          erule2 = EIRule "r7" [sym2] L2R False
          erule3 = EIRule "r8" [] R2L True

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
                                     TestLabel "Internal_asIRule_Derived" test38,
                                     TestLabel "toEDict_Empty_Left" test39,
                                     TestLabel "toEDict_NoMatch_Left" test40,
                                     TestLabel "toEDict_Match_Left_Size" test41,
                                     TestLabel "toEDict_Match_Left_Elt1" test42,
                                     TestLabel "toEDict_Match_Left_Etl2" test43,
                                     TestLabel "toEDict_Match_Left_Elt3" test44,
                                     TestLabel "toEDict_Empty_Right" test45,
                                     TestLabel "toEDict_NoMatch_Right" test46,
                                     TestLabel "toEDict_Match_Right_Size" test47,
                                     TestLabel "toEDict_Match_Right_Elt1" test48,
                                     TestLabel "toEDict_Match_Right_Elt2" test49,
                                     TestLabel "toIDict_Empty_Left" test50,
                                     TestLabel "toIDict_NoMatch_Left" test51,
                                     TestLabel "toIDict_Match_Left_Elt1" test52,
                                     TestLabel "toIDict_Match_Left_Elt2" test53,
                                     TestLabel "toIDict_Match_Left_Elt3" test54,
                                     TestLabel "toIDict_Empty_Right" test55,
                                     TestLabel "toIDict_NoMatch_Right" test56,
                                     TestLabel "toIDict_Match_Right_Elt1" test57,
                                     TestLabel "toIDict_Match_Right_Elt2" test58]

main = defaultMain tests
