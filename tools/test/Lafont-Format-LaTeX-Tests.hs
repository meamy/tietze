module Main where

import           Test.Framework
import           Test.Framework.Providers.HUnit
import           Test.HUnit
import           Lafont.Common
import           Lafont.Format.Common
import           Lafont.Format.LaTeX
import qualified Lafont.Generators.Semantics as Sem
import           Lafont.Rewrite.Common
import qualified Lafont.Rewrite.Lookup as Rel
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- Declares some symbols to ues throughout the test.

sym1 :: Symbol
sym1 = Symbol "abc_xyz" []

sym2 :: Symbol
sym2 = Symbol "gen0" []

sym3 :: Symbol
sym3 = Symbol "gen1" []

sym4 :: Symbol
sym4 = Symbol "gen2" []

sym5 :: Symbol
sym5 = Symbol "gen3" []

-----------------------------------------------------------------------------------------
-- makeGenMacros

gen1 :: (String, Maybe Int)
gen1 = ("abc_xyz", Just 1)

gen2 :: (String, Maybe Int)
gen2 = ("gen0", Just 2)

gen3 :: (String, Maybe Int)
gen3 = ("gen1", Nothing)

gen4 :: (String, Maybe Int)
gen4 = ("gen2", Nothing)

gen5 :: (String, Maybe Int)
gen5 = ("gen3", Nothing)

sampleGDict0 :: Sem.GenDict Int
sampleGDict0 = Sem.empty
sampleGDict1 = Sem.addGen sampleGDict0 gen1
sampleGDict2 = Sem.addGen sampleGDict1 gen2
sampleGDict3 = Sem.addGen sampleGDict2 gen3
sampleGDict4 = Sem.addGen sampleGDict3 gen4
sampleGDict5 = Sem.addGen sampleGDict4 gen5

test1 = TestCase (assertEqual "Can produce a empty MacroList from a GenDict."
                              ""
                              (printMacroList $ makeGenMacros syms))
      where syms = Sem.toAlphabet sampleGDict0

test2 = TestCase (assertEqual "Can produce a MacroList for a single generator."
                              (cmd1 ++ "\n")
                              (printMacroList $ makeGenMacros syms))
    where syms = Sem.toAlphabet sampleGDict1
          cmd1 = "% Macro for: abc_xyz" ++ "\n" ++ "\\newcommand{\\lftgenA}{X_{0}}"

test3 = TestCase (assertEqual "Can produce a MacroList for a set of generators."
                              (cmd1 ++ "\n" ++ cmd2 ++ "\n" ++ cmd3 ++ "\n")
                              (printMacroList $ makeGenMacros syms))
    where syms = Sem.toAlphabet sampleGDict3
          cmd1 = "% Macro for: abc_xyz" ++ "\n" ++ "\\newcommand{\\lftgenC}{X_{2}}"
          cmd2 = "% Macro for: gen0" ++ "\n" ++ "\\newcommand{\\lftgenB}{X_{1}}"
          cmd3 = "% Macro for: gen1" ++ "\n" ++ "\\newcommand{\\lftgenA}{X_{0}}"

-----------------------------------------------------------------------------------------
-- makeRelMacros

rule1 :: RewriteRule
rule1 = RewriteRule [sym1, sym2, sym3] [sym4] True (Primitive "rel1")

rule2 :: RewriteRule
rule2 = RewriteRule [sym2, sym2, sym2] [] True (Primitive "rel2")

rule3 :: RewriteRule
rule3 = RewriteRule [sym4, sym5] [sym5, sym4] True (Primitive "rel3")

rule4 :: RewriteRule
rule4 = RewriteRule [sym3] [sym1, sym2, sym4, sym5] True (Derived $ Just "drel1")

sampleRDict0 :: Rel.RuleDict
sampleRDict0 = Rel.empty
sampleRDict1 = Rel.addRule sampleRDict0 ("rel1", rule1)
sampleRDict2 = Rel.addRule sampleRDict1 ("rel2", rule2)
sampleRDict3 = Rel.addRule sampleRDict2 ("rel3", rule3)
sampleRDict4 = Rel.addRule sampleRDict3 ("drel1", rule4)

test4 = TestCase (assertEqual "Can produce a empty MacroList from a GenDict."
                              ""
                              (printMacroList $ makeRelMacros sampleRDict0))

test5 = TestCase (assertEqual "Can produce a MacroList for a single relation."
                              (cmd1 ++ "\n")
                              (printMacroList $ makeRelMacros sampleRDict1))
    where cmd1 = "% Macro for: rel1" ++ "\n" ++ "\\newcommand{\\lftrelA}{R_{0}}"

test6 = TestCase (assertEqual "Can produce a MacroList for a set of relations."
                              cmds
                              (printMacroList $ makeRelMacros sampleRDict4))
    where cmd1 = "% Macro for: drel1" ++ "\n" ++ "\\newcommand{\\lftrelD}{R_{3}}"
          cmd2 = "% Macro for: rel1" ++ "\n" ++ "\\newcommand{\\lftrelC}{R_{2}}"
          cmd3 = "% Macro for: rel2" ++ "\n" ++ "\\newcommand{\\lftrelB}{R_{1}}"
          cmd4 = "% Macro for: rel3" ++ "\n" ++ "\\newcommand{\\lftrelA}{R_{0}}"
          cmds = cmd1 ++ "\n" ++ cmd2 ++ "\n" ++ cmd3 ++ "\n" ++ cmd4 ++ "\n"

-----------------------------------------------------------------------------------------
-- printFormattedLine

gmacros :: MacroList
gmacros = makeGenMacros $ Sem.toAlphabet sampleGDict5

test7 = TestCase (assertEqual "Can print an empty FormattedLine."
                              "\\epsilon"
                              (printFormattedLine gmacros (NoEditLine [])))

test8 = TestCase (assertEqual "Can print an empty FormattedLine."
                              "\\lftgenE \\cdot \\lftgenC \\cdot \\lftgenD"
                              (printFormattedLine gmacros (NoEditLine word)))
    where word = [sym1, sym3, sym2]

-----------------------------------------------------------------------------------------
-- printFormattedStep

rmacros :: MacroList
rmacros = makeRelMacros sampleRDict4

test9 = TestCase (assertEqual "Can print an empty step (L2R)."
                              "\\xrightarrow{\\lftrelC} \\epsilon"
                              (printFormattedStep gmacros rmacros step))
    where name = Primitive "rel1"
          line = NoEditLine []
          step = FormattedStep name L2R line

test10 = TestCase (assertEqual "Can print an empty step (R2L)."
                               "\\xleftarrow{\\lftrelB} \\epsilon"
                               (printFormattedStep gmacros rmacros step))
    where name = Primitive "rel2"
          line = NoEditLine []
          step = FormattedStep name R2L line

test11 = TestCase (assertEqual "Can print an non-empty step (L2R)."
                               ("\\xrightarrow{\\lftrelA} " ++ mstr)
                               (printFormattedStep gmacros rmacros step))
    where name = Primitive "rel3"
          line = NoEditLine [sym1, sym3, sym2]
          step = FormattedStep name L2R line
          mstr = "\\lftgenE \\cdot \\lftgenC \\cdot \\lftgenD"

-----------------------------------------------------------------------------------------
-- printFormattedProof

line0a :: FormattedLine
line0a = NoEditLine [sym5, sym1, sym2, sym3]

line1a :: FormattedLine
line1a = NoEditLine [sym5, sym1, sym2, sym3, sym2, sym2, sym2]

line2a :: FormattedLine
line2a = NoEditLine [sym5, sym4, sym2, sym2, sym2]

line3a :: FormattedLine
line3a = NoEditLine [sym4, sym5, sym2, sym2, sym2]

step1a :: FormattedStep
step1a = FormattedStep (Primitive "rel2") L2R line1a

step2a :: FormattedStep
step2a = FormattedStep (Primitive "rel1") L2R line2a

step3a :: FormattedStep
step3a = FormattedStep (Primitive "rel3") R2L line3a

test12 = TestCase (assertEqual "Can print a zero-step proof."
                              latex
                              (printFormattedProof gmacros rmacros proof))
    where proof = FormattedProof line0a []
          latex = "\\begin{align*}\n" ++
                  "\\lftgenA \\cdot \\lftgenE \\cdot \\lftgenD \\cdot \\lftgenC\n" ++
                  "\\end{align*}"

test13 = TestCase (assertEqual "Can print a one-step proof."
                              latex
                              (printFormattedProof gmacros rmacros proof))
    where proof = FormattedProof line0a [step1a]
          sl1   = "\\lftgenA \\cdot \\lftgenE \\cdot \\lftgenD \\cdot \\lftgenC"
          sl2   = "\\lftgenD \\cdot \\lftgenD \\cdot \\lftgenD"
          latex =
            "\\begin{align*}\n" ++
            sl1 ++ "\n" ++
            "&\\xrightarrow{\\lftrelB} " ++ sl1 ++ " \\cdot " ++ sl2 ++ "\n" ++
            "\\end{align*}"

test14 = TestCase (assertEqual "Can print a two-step proof."
                              latex
                              (printFormattedProof gmacros rmacros proof))
    where proof = FormattedProof line0a [step1a, step2a]
          sl1   = "\\lftgenA \\cdot \\lftgenE \\cdot \\lftgenD \\cdot \\lftgenC"
          sl2   = "\\lftgenD \\cdot \\lftgenD \\cdot \\lftgenD"
          sl3   = "\\lftgenA \\cdot \\lftgenB"
          latex =
            "\\begin{align*}\n" ++
            sl1 ++ "\n" ++
            "&\\xrightarrow{\\lftrelB} " ++ sl1 ++ " \\cdot " ++ sl2 ++ " \\\\\n" ++
            "&\\xrightarrow{\\lftrelC} " ++ sl3 ++ " \\cdot " ++ sl2 ++ "\n" ++
            "\\end{align*}"

test15 = TestCase (assertEqual "Can print a three-step proof."
                              latex
                              (printFormattedProof gmacros rmacros proof))
    where proof = FormattedProof line0a [step1a, step2a, step3a]
          sl1   = "\\lftgenA \\cdot \\lftgenE \\cdot \\lftgenD \\cdot \\lftgenC"
          sl2   = "\\lftgenD \\cdot \\lftgenD \\cdot \\lftgenD"
          sl3   = "\\lftgenA \\cdot \\lftgenB"
          sl4   = "\\lftgenB \\cdot \\lftgenA"
          latex =
            "\\begin{align*}\n" ++
            sl1 ++ "\n" ++
            "&\\xrightarrow{\\lftrelB} " ++ sl1 ++ " \\cdot " ++ sl2 ++ " \\\\\n" ++
            "&\\xrightarrow{\\lftrelC} " ++ sl3 ++ " \\cdot " ++ sl2 ++ " \\\\\n" ++
            "&\\xleftarrow{\\lftrelA} " ++ sl4 ++ " \\cdot " ++ sl2 ++ "\n" ++
            "\\end{align*}"

test16 = TestCase (assertEqual "Can print a two-step proof."
                              latex
                              (printFormattedProof gmacros rmacros proof))
    where proof = FormattedProof line1a [step2a, step3a]
          sl1   = "\\lftgenA \\cdot \\lftgenE \\cdot \\lftgenD \\cdot \\lftgenC"
          sl2   = "\\lftgenD \\cdot \\lftgenD \\cdot \\lftgenD"
          sl3   = "\\lftgenA \\cdot \\lftgenB"
          sl4   = "\\lftgenB \\cdot \\lftgenA"
          latex =
            "\\begin{align*}\n" ++
            "&" ++ sl1 ++ " \\cdot " ++ sl2 ++ " \\\\\n" ++
            "&\\qquad\\xrightarrow{\\lftrelC} " ++ sl3 ++ " \\cdot " ++ sl2 ++ " \\\\\n" ++
            "&\\qquad\\xleftarrow{\\lftrelA} " ++ sl4 ++ " \\cdot " ++ sl2 ++ "\n" ++
            "\\end{align*}"

-----------------------------------------------------------------------------------------
-- printFormattedLine

test17 = TestCase (assertEqual "Can support ElimLine (1/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimLine [sym1] [sym2, sym3] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenC} " ++
                   "\\cdot \\lftgenA"

test18 = TestCase (assertEqual "Can support ElimLine (2/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimLine [] [sym2, sym3] [sym5]
            text = "\\underline{\\lftgenD \\cdot \\lftgenC} \\cdot \\lftgenA"

test19 = TestCase (assertEqual "Can support ElimLine (3/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimLine [sym1] [sym2, sym3] []
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenC}"

test20 = TestCase (assertEqual "Can support AddLine (1/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddLine [sym1] [sym2, sym3] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenC} " ++
                   "\\cdot \\lftgenA"

test21 = TestCase (assertEqual "Can support AddLine (2/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddLine [] [sym2, sym3] [sym5]
            text = "\\overline{\\lftgenD \\cdot \\lftgenC} \\cdot \\lftgenA"

test22 = TestCase (assertEqual "Can support AddLine (3/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddLine [sym1] [sym2, sym3] []
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenC}"

test23 = TestCase (assertEqual "Can support AddElimSplitLine (1/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddElimSplitLine [sym1] [sym2, sym2] [sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\underline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test24 = TestCase (assertEqual "Can support AddElimSplitLine (2/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddElimSplitLine [] [sym2, sym2] [sym3] [sym4, sym4] [sym5]
            text = "\\overline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\underline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test25 = TestCase (assertEqual "Can support AddElimSplitLine (3/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddElimSplitLine [sym1] [sym2, sym2] [] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\underline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test26 = TestCase (assertEqual "Can support AddElimSplitLine (4/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddElimSplitLine [sym1] [sym2, sym2] [sym3] [sym4, sym4] []
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\underline{\\lftgenB \\cdot \\lftgenB}"

test27 = TestCase (assertEqual "Can support ElimAddSplitLine (1/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimAddSplitLine [sym1] [sym2, sym2] [sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\overline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test28 = TestCase (assertEqual "Can support ElimAddSplitLine (2/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimAddSplitLine [] [sym2, sym2] [sym3] [sym4, sym4] [sym5]
            text = "\\underline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\overline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test29 = TestCase (assertEqual "Can support ElimAddSplitLine (3/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimAddSplitLine [sym1] [sym2, sym2] [] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\overline{\\lftgenB \\cdot \\lftgenB} \\cdot " ++
                   "\\lftgenA"

test30 = TestCase (assertEqual "Can support ElimAddSplitLine (4/4)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimAddSplitLine [sym1] [sym2, sym2] [sym3] [sym4, sym4] []
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD} \\cdot " ++
                   "\\lftgenC \\cdot \\overline{\\lftgenB \\cdot \\lftgenB}"

test31 = TestCase (assertEqual "Can support AddThenElimLine (1/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddThenElimLine [sym1] [sym2, sym3, sym4] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\overline{\\lftgenD \\cdot " ++
                   "\\lftgenC \\cdot \\lftgenB}} \\cdot \\lftgenA"

test32 = TestCase (assertEqual "Can support AddThenElimLine (2/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddThenElimLine [] [sym2, sym3, sym4] [sym5]
            text = "\\underline{\\overline{\\lftgenD \\cdot \\lftgenC \\cdot " ++
                   "\\lftgenB}} \\cdot \\lftgenA"

test33 = TestCase (assertEqual "Can support AddThenElimLine (3/3)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddThenElimLine [sym1] [sym2, sym3, sym4] []
            text = "\\lftgenE \\cdot \\underline{\\overline{\\lftgenD \\cdot " ++
                   "\\lftgenC \\cdot \\lftgenB}}"

test34 = TestCase (assertEqual "Can support ElimOverAddLine (1/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimOverAddLine [sym1] [sym2, sym2] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\overline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB} \\cdot \\lftgenA"

test35 = TestCase (assertEqual "Can support ElimOverAddLine (2/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimOverAddLine [] [sym2, sym2] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\underline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\overline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB} \\cdot \\lftgenA"

test36 = TestCase (assertEqual "Can support ElimOverAddLine (3/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimOverAddLine [sym1] [] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\overline{\\lftgenC \\cdot " ++
                   "\\lftgenC} \\cdot \\lftgenB \\cdot \\lftgenB} \\cdot \\lftgenA"

test37 = TestCase (assertEqual "Can support ElimOverAddLine (4/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimOverAddLine [sym1] [sym2, sym2] [sym3, sym3] [] [sym5]
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\overline{\\lftgenC \\cdot \\lftgenC}} \\cdot \\lftgenA"

test38 = TestCase (assertEqual "Can support ElimOverAddLine (5/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = ElimOverAddLine [sym1] [sym2, sym2] [sym3, sym3] [sym4, sym4] []
            text = "\\lftgenE \\cdot \\underline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\overline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB}"

test39 = TestCase (assertEqual "Can support AddOverElimLine (1/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddOverElimLine [sym1] [sym2, sym2] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\underline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB} \\cdot \\lftgenA"

test40 = TestCase (assertEqual "Can support AddOverElimLine (2/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddOverElimLine [] [sym2, sym2] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\overline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\underline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB} \\cdot \\lftgenA"

test41 = TestCase (assertEqual "Can support AddOverElimLine (3/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddOverElimLine [sym1] [] [sym3, sym3] [sym4, sym4] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\underline{\\lftgenC \\cdot " ++
                   "\\lftgenC} \\cdot \\lftgenB \\cdot \\lftgenB} \\cdot \\lftgenA"

test42 = TestCase (assertEqual "Can support AddOverElimLine (4/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddOverElimLine [sym1] [sym2, sym2] [sym3, sym3] [] [sym5]
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\underline{\\lftgenC \\cdot \\lftgenC}} \\cdot \\lftgenA"

test43 = TestCase (assertEqual "Can support AddOverElimLine (5/5)."
                               text
                               (printFormattedLine gmacros line))
      where line = AddOverElimLine [sym1] [sym2, sym2] [sym3, sym3] [sym4, sym4] []
            text = "\\lftgenE \\cdot \\overline{\\lftgenD \\cdot \\lftgenD \\cdot " ++
                   "\\underline{\\lftgenC \\cdot \\lftgenC} \\cdot \\lftgenB \\cdot " ++
                   "\\lftgenB}"

-----------------------------------------------------------------------------------------
-- Orchestrates tests.

tests = hUnitTestToTests $ TestList [TestLabel "makeGenMacros_Empty" test1,
                                     TestLabel "makeGenMacros_1Gen" test2,
                                     TestLabel "makeGenMacros_3Gen" test3,
                                     TestLabel "makeRelMacros_Empty" test4,
                                     TestLabel "makeRelMacros_1Rel" test5,
                                     TestLabel "makeRelMacros_4Rel" test6,
                                     TestLabel "printFormattedLine_Empty" test7,
                                     TestLabel "printFormattedLine_Nonempty" test8,
                                     TestLabel "printFormattedStep_Empty_L2R" test9,
                                     TestLabel "printFormattedStep_Empty_R2L" test10,
                                     TestLabel "printFormattedStep_Nonempty" test11,
                                     TestLabel "printFormattedProof_0Step" test12,
                                     TestLabel "printFormattedProof_1Step" test13,
                                     TestLabel "printFormattedProof_2Step" test14,
                                     TestLabel "printFormattedProof_3Step" test15,
                                     TestLabel "printFormattedProof_Long" test16,
                                     TestLabel "ElimLine_1" test17,
                                     TestLabel "ElimLine_2" test18,
                                     TestLabel "ElimLine_3" test19,
                                     TestLabel "AddLine_1" test20,
                                     TestLabel "AddLine_2" test21,
                                     TestLabel "AddLine_3" test22,
                                     TestLabel "AddElimSplitLine_1" test23,
                                     TestLabel "AddElimSplitLine_2" test24,
                                     TestLabel "AddElimSplitLine_3" test25,
                                     TestLabel "AddElimSplitLine_4" test26,
                                     TestLabel "ElimAddSplitLine_1" test27,
                                     TestLabel "ELimAddSplitLine_2" test28,
                                     TestLabel "ElimAddSplitLine_3" test29,
                                     TestLabel "ElimAddSplitLine_4" test30,
                                     TestLabel "AddThenElimLine_1" test31,
                                     TestLabel "AddThenElimLine_2" test32,
                                     TestLabel "AddThenElimLine_3" test33,
                                     TestLabel "ElimOverAddLine_1" test34,
                                     TestLabel "ElimOverAddLine_2" test35,
                                     TestLabel "ElimOverAddLine_3" test36,
                                     TestLabel "ElimOverAddLine_4" test37,
                                     TestLabel "ElimOverAddLine_5" test38,
                                     TestLabel "AddOverElimLine_1" test39,
                                     TestLabel "AddOverElimLine_2" test40,
                                     TestLabel "AddOverElimLine_3" test41,
                                     TestLabel "AddOverElimLine_4" test42,
                                     TestLabel "AddOverElimLine_5" test43]

main = defaultMain tests
