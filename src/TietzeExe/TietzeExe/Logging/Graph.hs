-- | Functions to log graph data (cycles, etc).

module TietzeExe.Logging.Graph
  ( printCycle
  , printUnmetDep
  ) where

-------------------------------------------------------------------------------
-- * Import Section.

import Tietze.Graph (foldPath)
import Tietze.Rewrite.Abstraction
  ( DepCycle (..)
  , UnmetDep (..)
  )

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
