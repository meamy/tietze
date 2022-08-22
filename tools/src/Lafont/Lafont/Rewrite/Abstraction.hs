-- | This module provides data types and functions to work with derivations before all
-- derived relation applications have been resolved.

module Lafont.Rewrite.Abstraction where

import           Data.Maybe
import           Data.Map as Map
import           Lafont.Graph
import           Lafont.Rewrite.Common
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * Derivations with Apply Types.

-- | Represents the instruction !appply <NAME> <DIR> <POS>. An apply is considered an
-- abstract derivational proof step, as it does not specify the terms to rewrite.
data Apply = Apply String RulePos RuleDir deriving (Eq,Show)

-- | Provides an abstract view of a derivational proof step. Each relation is either a
-- rewrite (i.e., a concrete step) or a derived relation application (i.e., an abstract
-- step).
type AbsRewrite = Either Rewrite Apply

-- | Provides an abstract view of a derivational proof. In an abstract proof, each step
-- is either a rewrite or the application of a derived relation. In other words, the
-- proof consists of abstract rewrites.
data AbsDerivation = AbsDerivation DerivationSummary [AbsRewrite] deriving (Eq,Show)

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
registerDerivations :: [AbsDerivation] -> DepGraph
registerDerivations []                = addVertex nullgraph ""
registerDerivations ((AbsDerivation summary _):list) =
    case propName $ meta summary of
        Just name -> addVertex g name
        Nothing   -> g
    where g = registerDerivations list

-- | Consumes a dependency (dep), a rewrite, and a dependency graph (g). If the rewrite
-- applies a derivation (dep') and at least one of dep or dep' is not a vertex in g, then
-- dep' is returned as an error. If both dep and dep' are in g, then the graph obtained
-- by adding an edge from dep to dep' is returned. If rewrite does not apply a
-- derivation, then g is returned.
addDepToGraph :: Dependency -> AbsRewrite -> DepGraph -> Either UnmetDep DepGraph
addDepToGraph src (Left Rewrite {})        g = Right g
addDepToGraph src (Right (Apply name _ _)) g =
    case addEdge g src name of
        Just g' -> Right g'
        Nothing -> Left (UnmetDep src name)

-- | Consumes a dependency (dep), a list of rewrites (rewrites), and a dependency graph
-- (g). If there exists a rewrite (r) in rewrites such that (addDepToGraph dep r g)
-- returns an error, then the first such error is returned. Otherwise, is equivalent to
-- folding rewrites using (addDepToGraph dep).
addDepsToGraph :: Dependency -> [AbsRewrite] -> DepGraph -> Either UnmetDep DepGraph
addDepsToGraph _   []                 g = Right g
addDepsToGraph src (rewrite:rewrites) g =
    case addDepToGraph src rewrite g of
        Left dep -> Left dep
        Right g' -> addDepsToGraph src rewrites g'

-- | Consumes a derivation and a graph. If the derivation is named, then returns the
-- result of addDepsToGraph using the name and rewrites of the derivation. Otherwise,
-- returns the results of addDepsToGraph using the name "" and the rewrites of the
-- derivation.
addDerivationToGraph :: AbsDerivation -> DepGraph -> Either UnmetDep DepGraph
addDerivationToGraph (AbsDerivation summary rewrites) g =
    case propName $ meta summary of
        Just src -> addDepsToGraph src rewrites g
        Nothing  -> addDepsToGraph "" rewrites g

-- | Consumes a list of derivations (derivations) and a graph (g). If there exists a
-- derivation (d) in rewrites such that (addDerivationToGraph d g) returns an error, then
-- the first such error is returns. Otherwise, is equivalent to folding derivations using
-- addDerivationToGraph.
addDerivationsToGraph :: [AbsDerivation] -> DepGraph -> Either UnmetDep DepGraph
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
detectDerivationError :: [AbsDerivation] -> Maybe (Either UnmetDep DepCycle)
detectDerivationError derivations =
    case addDerivationsToGraph derivations g of
        Left unmet -> Just (Left unmet)
        Right g'   -> case findCycle g' of
            Just cycle -> Just (Right cycle)
            Nothing    -> Nothing
    where g = registerDerivations derivations

-----------------------------------------------------------------------------------------
-- * Utilities to Look up Derivations by Name.

-- | A mapping from derivation names to their information.
type DerivationMap = Map.Map String AbsDerivation

-- | Attemtps to find a derivation of a given name.
getDerivation :: DerivationMap -> String -> Maybe AbsDerivation
getDerivation map name = name `Map.lookup` map

-- | Consumes  list of abstract derivations. Returns a derivation map containing an entry
-- for each named derivation in the list.
makeDerivationMap :: [AbsDerivation] -> DerivationMap
makeDerivationMap []                       = Map.empty
makeDerivationMap (derivation:derivations) =
    case propName $ meta summary of
        Just name -> Map.insert name derivation dict
        Nothing   -> dict
    where (AbsDerivation summary _) = derivation
          dict = makeDerivationMap derivations
