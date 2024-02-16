module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Common
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

app1a = Rewrite rule1 2 L2R
app2a = Rewrite rule4 0 R2L
app3a = Rewrite rule3 1 R2L
app4a = Rewrite rule2 3 L2R

-- Lines

word_a :: MonWord
word_a = [sym1, sym2, sym1, sym2, sym3, sym5, sym5, sym4, sym2, sym2, sym2]

line0a :: FormattedLine
line0a = NoEditLine word_a

line1a :: FormattedLine
line1a = NoEditLine [sym1, sym2, sym4, sym5, sym5, sym4, sym2, sym2, sym2]

line2a :: FormattedLine
line2a = NoEditLine [sym3, sym5, sym4, sym2, sym2, sym2]

line3a :: FormattedLine
line3a = NoEditLine [sym3, sym4, sym5, sym2, sym2, sym2]

line4a :: FormattedLine
line4a = NoEditLine [sym3, sym4, sym5]

-- Steps

step1a :: FormattedStep
step1a = FormattedStep (Primitive "rel1") L2R line1a

step2a :: FormattedStep
step2a = FormattedStep (Derived $ Just "drel1") R2L line2a

step3a :: FormattedStep
step3a = FormattedStep (Primitive "rel3") R2L line3a

step4a :: FormattedStep
step4a = FormattedStep (Primitive "rel2") L2R line4a

-- Tests

test1 = TestCase (assertEqual "Can handle an empty derivation."
                              (FormattedProof line0a [])
                              (formatProof word_a []))

test2 = TestCase (assertEqual "Can handle a 1-step derivation."
                              (FormattedProof line0a [step1a])
                              (formatProof word_a [app1a]))

test3 = TestCase (assertEqual "Can handle a 2-step derivation."
                              (FormattedProof line0a [step1a, step2a])
                              (formatProof word_a [app1a, app2a]))

test4 = TestCase (assertEqual "Can handle a 3-step derivation."
                              (FormattedProof line0a [step1a, step2a, step3a])
                              (formatProof word_a [app1a, app2a, app3a]))

test5 = TestCase (assertEqual "Can handle a 4-step derivation."
                              (FormattedProof line0a [step1a, step2a, step3a, step4a])
                              (formatProof word_a [app1a, app2a, app3a, app4a]))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "formatProof_Empty" test1,
                                     TestLabel "formatProof_1Step" test2,
                                     TestLabel "formatProof_2Step" test3,
                                     TestLabel "formatProof_3Step" test4,
                                     TestLabel "formatProof_4Step" test5]

main = defaultMain tests
