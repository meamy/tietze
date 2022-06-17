module Main where

import Test.Framework
import Test.Framework.Providers.HUnit
import Test.HUnit
import Lafont.Common
import Lafont.Rewrite.Rules
import LafontExe.IO.PrimitiveLogging

-----------------------------------------------------------------------------------------
-- logWord

word1 :: MonWord
word1 = []

word2 :: MonWord
word2 = [(Symbol "xyz" [])]

word3 :: MonWord
word3 = [(Symbol "xyz" []), (Symbol "qrs" []), (Symbol "qwerty" [])]

word4 :: MonWord
word4 = [(Symbol "xyz" []), (Symbol "qrs" [5]), (Symbol "qwerty" [10, 20])]


test1 = TestCase (assertEqual "logRule can format empty words."
                              "ε"
                              (logWord word1))

test2 = TestCase (assertEqual "logRule can format a one symbol word."
                              "xyz"
                              (logWord word2))

test3 = TestCase (assertEqual "logRule can format a three symbol word."
                              "xyz.qrs.qwerty"
                              (logWord word3))

test4 = TestCase (assertEqual "logRule can format a three symbol word with parameters."
                              "xyz.qrs[5].qwerty[10][20]"
                              (logWord word4))

-----------------------------------------------------------------------------------------
-- logRule

rule1 :: RewriteRule
rule1 = RewriteRule word1 word2 True False

rule2 :: RewriteRule
rule2 = RewriteRule word2 word3 True False

rule3 :: RewriteRule
rule3 = RewriteRule word1 word2 False False

rule4 :: RewriteRule
rule4 = RewriteRule word1 word2 True True

test5 = TestCase (assertEqual "logRule can display equational rules (1/2)."
                              "rel1: ε = xyz"
                              (logRule ("rel1", rule1)))

test6 = TestCase (assertEqual "logRule can display equational rules (2/2)."
                              "relation2: xyz = xyz.qrs.qwerty"
                              (logRule ("relation2", rule2)))

test7 = TestCase (assertEqual "logRule can display production rules."
                              "rule3: ε → xyz"
                              (logRule ("rule3", rule3)))

test8 = TestCase (assertEqual "logRule can display derived rules."
                              "rel4: ε = xyz"
                              (logRule ("rel4", rule1)))

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "logWord_EmptyString" test1,
                                     TestLabel "logWord_OneSymbol" test2,
                                     TestLabel "logWord_ManySymbols" test3,
                                     TestLabel "logWord_Parameters" test4,
                                     TestLabel "logRule_EquationalOne" test5,
                                     TestLabel "logRule_EquationalTwo" test6,
                                     TestLabel "logRule_ProductionRule" test7,
                                     TestLabel "logRule_Derived" test8]

main = defaultMain tests
