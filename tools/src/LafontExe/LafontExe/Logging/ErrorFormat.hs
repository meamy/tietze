-- | Functions to format error messages.

module LafontExe.Logging.ErrorFormat (
    describeFailedApply,
    describeIncorrectResult,
    describeIncorrectStep,
    reportMissingERule,
    reportMissingIRule,
    reportDupRule,
    reportInvalidRule,
    reportUnknownGen
) where

import           Lafont.Common
import           LafontExe.Logging.Primitive

-----------------------------------------------------------------------------------------
-- * Relation Error Logging.

-- | Consumes the name of a rule, and returns an error message stating that the rule is
-- semantically invalid.
reportInvalidRule :: String -> String
reportInvalidRule rname = "Rule contradicts semantics: " ++ rname ++ "\n"

-- | Consumes the name of a rule, and returns an error message stating that the rule
-- contains an unknown generator.
reportUnknownGen :: String -> String
reportUnknownGen rname = "Rule contains an unknown generator: " ++ rname ++ "\n"

-----------------------------------------------------------------------------------------
-- * Derivation Error Logging.

-- | Consumes the name of a derivation file (fname) and the index of a derivation (num).
-- Returns the first line of a derivation validation error.
prefaceDerivationError :: String -> Int -> String
prefaceDerivationError fname num = "Failed to validate " ++ der ++ " in " ++ fname ++ "."
    where index = show num
          der = "derivation(" ++ index ++ ")"

-- | Consumes the name of a derivation file (fname), the index of a derivation (num), the
-- word obtained from a derivation (act), and the expected word from the end of the file
-- (exp). Returns a string describing the error.
describeIncorrectResult :: String -> Int -> MonWord -> MonWord -> String
describeIncorrectResult fname num exp act = fstLine ++ "\n" ++ sndLine ++ "\n"
    where expStr = logWord exp
          actStr = logWord act
          fstLine = prefaceDerivationError fname num
          sndLine = "Expected " ++ expStr ++ " but produced " ++ actStr ++ "."

-- | Consumes the name of a derivation file (fname), the index of a derivation (num), the
-- word obtain when a rewrite rule failed to apply (act), and the step number associated
-- with this rewrite (step). Returns a string describing the error.
describeIncorrectStep :: String -> Int -> MonWord -> Int -> String
describeIncorrectStep fname num act step = fstLine ++ "\n" ++ sndLine ++ "\n"
    where actStr = logWord act
          stepStr = show step
          fstLine = prefaceDerivationError fname num
          sndLine = "Obtained " ++ actStr ++ " at step " ++ stepStr ++ "."

-- | Consumes the name of a derivation file (fname), the index of a derivation (num), and
-- the proof step at which an apply was not concretizable. Returns a string describing
-- the error.
describeFailedApply :: String -> Int -> Int -> String
describeFailedApply fname num pos = fstLine ++ "\n" ++ sndLine ++ "\n" ++ thdLine ++ "\n"
    where step = show pos
          fstLine = prefaceDerivationError fname num
          sndLine = "The derivation applied at step " ++ step ++ " is not equational."
          thdLine = "However, the rule is applied right-to-left."

-- | Consumes the index of a derivation (num). Returns a message stating that this
-- derivation has a duplication name.
reportDupRule :: Int -> String
reportDupRule num = "Rule at index " ++ show num ++ " has duplicate name."

-----------------------------------------------------------------------------------------
-- * EIRule Error Logging.

-- | Consumes an EIRule type (ty) and a symbol (sym). Returns a line stating that a rule
-- of type ty could not be found for sym.
reportMissingEIRule :: String -> Symbol -> String
reportMissingEIRule ty sym = "Unable to find " ++ ty ++ " rule for " ++ symstr ++ ".\n"
    where symstr = display sym

-- | Specializes reportMissingEIRule for ty=elimination.
reportMissingERule :: Symbol -> String
reportMissingERule = reportMissingEIRule "elimination"

-- | Specializes reportMissingEIRule for ty=introduction.
reportMissingIRule :: Symbol -> String
reportMissingIRule = reportMissingEIRule "introduction"
