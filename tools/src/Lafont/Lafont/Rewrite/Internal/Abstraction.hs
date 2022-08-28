-- | Internals for Abstraction. Enables unit testing.

module Lafont.Rewrite.Internal.Abstraction (
    Apply ( .. ),
    DepGraph ( .. ),
    UnmetDep ( .. ),
    AbsRewrite,
    Dependency,
    addDepToGraph,
    addDepsToGraph
) where

import           Lafont.Either
import           Lafont.Graph
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules

-----------------------------------------------------------------------------------------
-- * Derivations with Apply Types.

-- | Represents the instruction !appply <NAME> <DIR> <POS>. An apply is considered an
-- abstract derivational proof step, as it does not specify the terms to rewrite.
data Apply = Apply String RulePos RuleDir deriving (Eq,Show)

-- | Provides an abstract view of a derivational proof step. Each relation is either a
-- rewrite (i.e., a concrete step) or a derived relation application (i.e., an abstract
-- step).
type AbsRewrite = Either Rewrite Apply

-----------------------------------------------------------------------------------------
-- * Derivation to Graph Conversion

-- | A vertex in a dependency graph.
type Dependency = String

-- | A graph to store dependencies between derivations.
newtype DepGraph = DepGraph (Digraph Dependency) deriving (Eq,Show)

-- | A derivation and its unmet dependency.
data UnmetDep = UnmetDep Dependency Dependency deriving (Eq,Show)

-- | Consumes a dependency (dep), a rewrite, and a dependency graph (g). If the rewrite
-- applies a derivation (dep') and at least one of dep or dep' is not a vertex in g, then
-- dep' is returned as an error. If both dep and dep' are in g, then the graph obtained
-- by adding an edge from dep to dep' is returned. If rewrite does not apply a
-- derivation, then g is returned.
addDepToGraph :: Dependency -> AbsRewrite -> DepGraph -> Either UnmetDep DepGraph
addDepToGraph src (Left Rewrite {})        g            = Right g
addDepToGraph src (Right (Apply name _ _)) (DepGraph g) =
    case addEdge g src name of
        Just g' -> Right (DepGraph g')
        Nothing -> Left (UnmetDep src name)

-- | Consumes a dependency (dep), a list of rewrites (rewrites), and a dependency graph
-- (g). If there exists a rewrite (r) in rewrites such that (addDepToGraph dep r g)
-- returns an error, then the first such error is returned. Otherwise, is equivalent to
-- folding rewrites using (addDepToGraph dep).
addDepsToGraph :: Dependency -> [AbsRewrite] -> DepGraph -> Either UnmetDep DepGraph
addDepsToGraph _   []                 g = Right g
addDepsToGraph src (rewrite:rewrites) g = branchRight (addDepToGraph src rewrite g)
                                                      (addDepsToGraph src rewrites)
