-- | This module provides data types to store derivation files, and functions to analyze
-- the dependencies of derivation files.

module Lafont.Rewrite.Derivations where

import           Lafont.Graph
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Types to Represent a Derivation

-- | A concrete description of a derivation (i.e., includes all rewrite data).
data Derivation = Derivation { summary  :: DerivationSummary
                             , rewrites :: [Rewrite]
                             } deriving (Eq,Show)

-----------------------------------------------------------------------------------------
-- * Derivation to Graph Conversion

-- | A vertex in a dependency graph.
type Dependency = String

-- | A graph to store dependencies between derivations.
type DepGraph = Digraph Dependency

-- | A derivation and its unmet dependency.
data UnmetDep = UnmetDep Dependency Dependency deriving (Eq,Show)

-- | Consumes a list of derivations (list). Returns a dependency graph with a vertex for
-- each derivation in list. A special vertex is added for unnamed derivations ("").
registerDerivations :: [Derivation] -> DepGraph
registerDerivations []                = addVertex nullgraph ""
registerDerivations (derivation:list) =
    case propName $ meta $ summary derivation of
        Just name -> addVertex g name
        Nothing   -> g
    where g = registerDerivations list

-- | Consumes a dependency (dep), a rewrite, and a dependency graph (g). If the rewrite
-- applies a derivation (dep') and at least one of dep or dep' is not a vertex in g, then
-- dep' is returned as an error. If both dep and dep' are in g, then the graph obtained
-- by adding an edge from dep to dep' is returned. If rewrite does not apply a
-- derivation, then g is returned.
addDepToGraph :: Dependency -> Rewrite -> DepGraph -> Either UnmetDep DepGraph
addDepToGraph src rewrite g =
    case derivedFrom $ rule rewrite of
        Just dep -> case addEdge g src dep of
            Just g' -> Right g'
            Nothing -> Left (UnmetDep src dep)
        Nothing -> Right g

-- | Consumes a dependency (dep), a list of rewrites (rewrites), and a dependency graph
-- (g). If there exists a rewrite (r) in rewrites such that (addDepToGraph dep r g)
-- returns an error, then the first such error is returned. Otherwise, is equivalent to
-- folding rewrites using (addDepToGraph dep).
addDepsToGraph :: Dependency -> [Rewrite] -> DepGraph -> Either UnmetDep DepGraph
addDepsToGraph _   []                 g = Right g
addDepsToGraph src (rewrite:rewrites) g =
    case addDepToGraph src rewrite g of
        Left dep -> Left dep
        Right g' -> addDepsToGraph src rewrites g'

-- | Consumes a derivation and a graph. If the derivation is named, then returns the
-- result of addDepsToGraph using the name and rewrites of the derivation. Otherwise,
-- returns the results of addDepsToGraph using the name "" and the rewrites of the
-- derivation.
addDerivationToGraph :: Derivation -> DepGraph -> Either UnmetDep DepGraph
addDerivationToGraph derivation g =
    case propName $ meta $ summary derivation of
        Just src -> addDepsToGraph src (rewrites derivation) g
        Nothing  -> addDepsToGraph "" (rewrites derivation) g

-- | Consumes a list of derivations (derivations) and a graph (g). If there exists a
-- derivation (d) in rewrites such that (addDerivationToGraph d g) returns an error, then
-- the first such error is returns. Otherwise, is equivalent to folding derivations using
-- addDerivationToGraph.
addDerivationsToGraph :: [Derivation] -> DepGraph -> Either UnmetDep DepGraph
addDerivationsToGraph []                       g = Right g
addDerivationsToGraph (derivation:derivations) g =
    case addDerivationToGraph derivation g of
        Left dep -> Left dep
        Right g' -> addDerivationsToGraph derivations g'

-----------------------------------------------------------------------------------------
-- * Cycle Detection

-- | A sequence of derivations that depend on one-another
type DepCycle = GraphWalk Dependency

-- | Consumes a list of derivations. If the list contains an unmet dependency (i.e., one
-- derivation calls an unknown derivation), then just the name of the dependency is
-- returned. If the list contains a dependency cycle, then just the cycle is returned.
-- Otherwise, nothing is returned.
detectDerivationError :: [Derivation] -> Maybe (Either UnmetDep DepCycle)
detectDerivationError derivations =
    case addDerivationsToGraph derivations g of
        Left unmet -> Just (Left unmet)
        Right g'   -> case findCycle g' of
            Just cycle -> Just (Right cycle)
            Nothing    -> Nothing
    where g = registerDerivations derivations
