-- | Functions to format error messages.

module LafontExe.Logging.ErrorFormat where

import           Lafont.Common
import           LafontExe.Logging.Primitive

-----------------------------------------------------------------------------------------
-- * Miscellaneous Error Logging.

-- | Consumes the name of a derivation file (fname), the index of a derivation (num), the
-- word obtained from a derivation (act), and the expected word from the end of the file
-- (exp). Returns a string describing the error.
describeIncorrectResult :: String -> Int -> MonWord -> MonWord -> String
describeIncorrectResult fname num exp act = fstLine ++ "\n" ++ sndLine ++ "\n"
    where expStr = logWord exp
          actStr = logWord act
          index = show num
          fstLine = "Failed to validate derivation(" ++ index ++ ") in " ++ fname ++ "."
          sndLine = "Expected " ++ expStr ++ " but produced " ++ actStr ++ "."

-- | Consumes the name of a derivation file (fname), the index of a derivation (num), the
-- word obtain when a rewrite rule failed to apply (act), and the step number associated
-- with this rewrite (step). Returns a string describing the error.
describeIncorrectStep :: String -> Int -> MonWord -> Int -> String
describeIncorrectStep fname num act step = fstLine ++ "\n" ++ sndLine ++ "\n"
    where actStr = logWord act
          stepStr = show step
          index = show num
          fstLine = "Failed to validate derivation(" ++ index ++ ") in " ++ fname ++ "."
          sndLine = "Obtained " ++ actStr ++ " at step " ++ stepStr ++ "."

-- | Consumes the index of a derivation (num). Returns a message stating that this
-- derivation has a duplication name.
reportDupRule :: Int -> String
reportDupRule num = "Rule at index " ++ show num ++ " has duplicate name."

-- | Consumes the name of a rule, and returns an error message stating that the rule is
-- semantically invalid.
reportInvalidRule :: String -> String
reportInvalidRule rname = "Rule contradicts semantics: " ++ rname ++ "\n"

-- | Consumes the name of a rule, and returns an error message stating that the rule
-- contains an unknown generator.
reportUnknownGen :: String -> String
reportUnknownGen rname = "Rule contains an unknown generator: " ++ rname ++ "\n"
