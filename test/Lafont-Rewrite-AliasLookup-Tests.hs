module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Tietze.Common
import Tietze.Rewrite.AliasLookup
import Tietze.Rewrite.Common
import Tietze.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Rule Dictionary Preparation.

-- Alphabet
sym_a = Symbol "a" []
sym_b = Symbol "b" []
sym_c = Symbol "c" []
sym_d = Symbol "d" []
sym_x = Symbol "x" []
sym_y = Symbol "y" []
sym_z = Symbol "z" []
alpha = map name [sym_a, sym_b, sym_c, sym_d, sym_x, sym_y, sym_z]

-- Acyclic Alias.
rel1 = RewriteRule [sym_a] [sym_x, sym_y] True (Primitive "rel1")
rel2 = RewriteRule [sym_b] [sym_y, sym_y, sym_z] True (Primitive "rel2")
rel3 = RewriteRule [sym_c] [sym_a, sym_b, sym_y] True (Primitive "rel3")
rel4 = RewriteRule [sym_d] [sym_a, sym_b, sym_c, sym_a, sym_b] True (Primitive "rel4")

-- Additional Cyclic Alias.
rel5 = RewriteRule [sym_x] [sym_y, sym_c, sym_y] True (Primitive "rel5")
rel6 = RewriteRule [sym_x] [sym_x] True (Primitive "rel6")

-----------------------------------------------------------------------------------------
-- addAlias: valid.

checkValidity :: Either AddAliasFailure AliasLookup -> AliasLookup
checkValidity (Right x) = x
checkValidity _         = error "Invalid call to addAlias."

-- Acyclic Alias Lookups.
lookup0 = toAliasLookup alpha
lookup1 = checkValidity $ addAlias lookup0 rel1
lookup2 = checkValidity $ addAlias lookup1 rel2
lookup3 = checkValidity $ addAlias lookup2 rel3
lookup4 = checkValidity $ addAlias lookup3 rel4

-- Cyclic Alias Lookups.
cyclic1 = checkValidity $ addAlias lookup4 rel5
cyclic2 = checkValidity $ addAlias lookup4 rel6

-----------------------------------------------------------------------------------------
-- addAlias: invalid.

test1 = TestCase (assertEqual "addAlias: The left-hand side must be a single symbol."
                              (Left InvalidLHS)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [sym_x, sym_x] [sym_y] True (Primitive "bad")

test2 = TestCase (assertEqual "addAlias: The left-hand side must be parameter-free."
                              (Left InvalidLHS)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [Symbol "z" [5]] [sym_x] True (Primitive "bad")

test3 = TestCase (assertEqual "addAlias: The right-hand side must be parameter-free."
                              (Left InvalidRHS)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [sym_x] [Symbol "z" [5]] True (Primitive "bad")

test4 = TestCase (assertEqual "addAlias: A symbol may have at most one alias."
                              (Left DuplicateAlias)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [sym_a] [sym_x, sym_x, sym_x] True (Primitive "bad")

test5 = TestCase (assertEqual "addAlias: All symbols must appear in the alphabet."
                              (Left UnexpectedSymbol)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [Symbol "q" []] [sym_x] True (Primitive "bad")

test6 = TestCase (assertEqual "addAlias: Alias rules must be bidirectional."
                              (Left DirectedAlias)
                              (addAlias lookup4 rel))
    where rel = RewriteRule [sym_x] [sym_y] False (Primitive "bad")

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "addAlias_InvalidLHS_1" test1,
                                     TestLabel "addAlias_InvalidLHS_2" test2,
                                     TestLabel "addAlias_InvalidRHS" test3,
                                     TestLabel "addAlias_DuplicateAlias" test4,
                                     TestLabel "addAlias_UnexpectedSymbol" test5,
                                     TestLabel "addAlias_DirectedAlias" test6]

main = defaultMain tests
