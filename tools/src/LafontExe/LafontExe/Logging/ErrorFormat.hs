-- | Functions to format error messages.

module LafontExe.Logging.ErrorFormat where

import           Lafont.Common
import           LafontExe.Logging.Primitive

-----------------------------------------------------------------------------------------
-- * Miscellaneous Error Logging.

-- | Consumes the name of a derivation file (fname), the word obtained from a derivation
-- (act), and the expected word from the end of the file (exp). Returns a string
-- describing the error.
describeIncorrectResult :: String -> MonWord -> MonWord -> String
describeIncorrectResult fname exp act = fstLine ++ sndLine
    where expStr = logWord exp
          actStr = logWord act
          fstLine = "Failed to validate " ++ fname ++ ".\n"
          sndLine = "Expected " ++ expStr ++ " but produced " ++ actStr ++ ".\n"

-- | Consumes the name of a derivation file (fname), the word obtain when a rewrite rule
-- failed to apply (act), and the step number associated with this rewrite (step).
-- Returns a string describing the error.
describeIncorrectStep :: String -> MonWord -> Int -> String
describeIncorrectStep fname act step = fstLine ++ sndLine
    where actStr = logWord act
          stepStr = show step
          fstLine = "Failed to validate " ++ fname ++ ".\n"
          sndLine = "Obtained " ++ actStr ++ " at step " ++ stepStr ++ ".\n"

-- | Consumes the name of a rule, and returns an error message stating that the rule is
-- semantically invalid.
reportInvalidRule :: String -> String
reportInvalidRule rname = "Rule contradicts semantics: " ++ rname ++ "\n"

-- | Consumes the name of a rule, and returns an error message stating that the rule
-- contains an unknown generator.
reportUnknownGen :: String -> String
reportUnknownGen rname = "Rule contains an unknown generator: " ++ rname ++ "\n"
