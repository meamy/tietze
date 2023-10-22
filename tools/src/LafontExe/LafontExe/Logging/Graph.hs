-- | Functions to log graph data (cycles, etc).

module LafontExe.Logging.Graph where

import           Lafont.Graph
import           Lafont.Rewrite.Abstraction

-----------------------------------------------------------------------------------------
-- * Logging Cycles/Dependencies.

-- | Displays a cycle as a humnan-readable list. The list starts on a new line.
printCycle :: DepCycle -> String
printCycle = foldPath f ""
    where f n v str = "\n" ++ show (n + 1) ++ ". " ++ v ++ str

-- | Displays an unmet dependency as a human-readable string.
printUnmetDep :: UnmetDep -> String
printUnmetDep (UnmetDep ""  dst) = dst
printUnmetDep (UnmetDep src dst) = src ++ " -> " ++ dst
