module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Common
import Lafont.Rewrite.Derivations
import Lafont.Rewrite.Summary
import Lafont.Format.Common
import Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Declares some symbols to ues throughout the test.

sym1 :: Symbol
sym1 = Symbol "A" []

sym2 :: Symbol
sym2 = Symbol "B" []

sym3 :: Symbol
sym3 = Symbol "C" []

sym4 :: Symbol
sym4 = Symbol "D" []

sym5 :: Symbol
sym5 = Symbol "E" []

-----------------------------------------------------------------------------------------
-- FormattedProof Tests

-- Rules

rule1 :: RewriteRule
rule1 = RewriteRule [sym1, sym2, sym3] [sym4] True (Primitive "rel1")

rule2 :: RewriteRule
rule2 = RewriteRule [sym2, sym2, sym2] [] True (Primitive "rel2")

rule3 :: RewriteRule
rule3 = RewriteRule [sym4, sym5] [sym5, sym4] True (Primitive "rel3")

rule4 :: RewriteRule
rule4 = RewriteRule [sym3] [sym1, sym2, sym4, sym5] True (Derived $ Just "drel1")

-- Applications

app1 = Rewrite rule1 2 L2R
app2 = Rewrite rule4 0 R2L
app3 = Rewrite rule3 1 R2L
app4 = Rewrite rule2 3 L2R

-- Initial Word

word_a :: MonWord
word_a = [sym1, sym2, sym1, sym2, sym3, sym5, sym5, sym4, sym2, sym2, sym2]

-- Test 1

line0a :: FormattedLine
line0a = NoEditLine word_a

test1 = TestCase (assertEqual "Can handle an empty derivation."
                              (FormattedProof line0a [])
                              (formatProof word_a []))

-- Test 2

line0b :: FormattedLine
line0b = ElimLine [sym1, sym2] [sym1, sym2, sym3] [sym5, sym5, sym4, sym2, sym2, sym2]

line1b :: FormattedLine
line1b = AddLine [sym1, sym2] [sym4] [sym5, sym5, sym4, sym2, sym2, sym2]

step1b :: FormattedStep
step1b = FormattedStep (Primitive "rel1") L2R line1b

test2 = TestCase (assertEqual "Can handle a 1-step derivation."
                              (FormattedProof line0b [step1b])
                              (formatProof word_a [app1]))

-- Test 3

line1c :: FormattedLine
line1c = NoEditLine [sym1, sym2, sym4, sym5, sym5, sym4, sym2, sym2, sym2]

line2c :: FormattedLine
line2c = AddLine [] [sym3] [sym5, sym4, sym2, sym2, sym2]

step1c :: FormattedStep
step1c = FormattedStep (Primitive "rel1") L2R line1c

step2c :: FormattedStep
step2c = FormattedStep (Derived $ Just "drel1") R2L line2c

test3 = TestCase (assertEqual "Can handle a 2-step derivation."
                              (FormattedProof line0b [step1c, step2c])
                              (formatProof word_a [app1, app2]))

-- Test 4

line2d :: FormattedLine
line2d = AddElimSplitLine [] [sym3] [] [sym5, sym4] [sym2, sym2, sym2]

line3d :: FormattedLine
line3d = AddLine [sym3] [sym4, sym5] [sym2, sym2, sym2]

step2d :: FormattedStep
step2d = FormattedStep (Derived $ Just "drel1") R2L line2d

step3d :: FormattedStep
step3d = FormattedStep (Primitive "rel3") R2L line3d

test4 = TestCase (assertEqual "Can handle a 3-step derivation."
                              (FormattedProof line0b [step1c, step2d, step3d])
                              (formatProof word_a [app1, app2, app3]))

-- Test 5

line3e :: FormattedLine
line3e = AddElimSplitLine [sym3] [sym4, sym5] [] [sym2, sym2, sym2] []

line4e :: FormattedLine
line4e = NoEditLine [sym3, sym4, sym5]

step3e :: FormattedStep
step3e = FormattedStep (Primitive "rel3") R2L line3e

step4e :: FormattedStep
step4e = FormattedStep (Primitive "rel2") L2R line4e

test5 = TestCase (assertEqual "Can handle a 4-step derivation."
                              (FormattedProof line0b [step1c, step2d, step3e, step4e])
                              (formatProof word_a [app1, app2, app3, app4]))

-----------------------------------------------------------------------------------------
-- formatDerivation Tests

word_b :: MonWord 
word_b = [sym3, sym4, sym5]

test6 = TestCase (assertEqual "Can convert derivations to proofs."
                              (FormattedProof line0b [step1c, step2d, step3e, step4e])
                              (formatDerivation der))
    where sum = DerivationSummary (RewritePreamble Nothing Nothing) word_a word_b
          der = Derivation sum [app1, app2, app3, app4]

-----------------------------------------------------------------------------------------
-- Alternative Format Tests

-- ElimAddSplitLine

word_c :: MonWord
word_c = [sym2, sym2, sym2]

app5 = Rewrite rule2 3 R2L
app6 = Rewrite rule2 0 L2R

line0f :: FormattedLine
line0f = NoEditLine [sym2, sym2, sym2]

line5f :: FormattedLine
line5f = ElimAddSplitLine [] [sym2, sym2, sym2] [] [sym2, sym2, sym2] []

line6f :: FormattedLine
line6f = NoEditLine [sym2, sym2, sym2]

step5f :: FormattedStep
step5f = FormattedStep (Primitive "rel2") R2L line5f

step6f :: FormattedStep
step6f = FormattedStep (Primitive "rel2") L2R line6f

test7 = TestCase (assertEqual "Can handle ElimAddSplitLine format."
                              (FormattedProof line0f [step5f, step6f])
                              (formatProof word_c [app5, app6]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "formatProof_Empty" test1,
                                     TestLabel "formatProof_1Step" test2,
                                     TestLabel "formatProof_2Step" test3,
                                     TestLabel "formatProof_3Step" test4,
                                     TestLabel "formatProof_4Step" test5,
                                     TestLabel "formatDerivation_Test" test6,
                                     TestLabel "ElimAddSplitLine" test7]

main = defaultMain tests
