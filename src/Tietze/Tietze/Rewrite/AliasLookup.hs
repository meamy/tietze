-- | This module provides data types and functions to determine if a set of alias
-- relations form an acyclic derivation graph.

module Tietze.Rewrite.AliasLookup
  ( AddAliasFailure (..)
  , AliasLookup
  , addAlias
  , toAliasLookup
  ) where

-----------------------------------------------------------------------------------------
-- * Import Section.

import qualified Data.Map as Map

import Tietze.Common
  ( MonWord
  , Symbol (..)
  )
import Tietze.Graph
  ( Digraph
  , addEdge
  , addVertex
  , nullgraph
  )
import Tietze.Maybe (branchJust)
import Tietze.Rewrite.Rules (RewriteRule (..))

-----------------------------------------------------------------------------------------
-- * Error Types.

-- | Error types for adding aliases to an Alias Table.
data AddAliasFailure = InvalidLHS
                     | InvalidRHS
                     | DuplicateAlias
                     | UnexpectedSymbol
                     | DirectedAlias
                     deriving (Show, Eq)

-----------------------------------------------------------------------------------------
-- * Alias Lookup.

-- | Internal type for mapping parameter-free symbols to parameter-free words.
type AliasTable = Map.Map String [String]

-- | Internal type for recording dependencies between symbols.
type AliasDeps = Digraph String

-- | Maintains a collection of aliases, and their dependency graph. Each symbol may have
-- at most one alias. Cyclic dependencies are allowed, but may lead to errors at later
-- stages of alias analysis.
data AliasLookup = AliasLookup AliasTable AliasDeps deriving (Show, Eq)

-- | Constructs an alias lookup for an alphabet. By default, symbols do not have aliases.
toAliasLookup :: [String] -> AliasLookup
toAliasLookup alpha = AliasLookup Map.empty $ foldl addVertex nullgraph alpha

-- | Helper function to strip arguments from a MonWord. If a symbol is parameterized,
-- then this function fails and returns nothing.
stripArgs :: MonWord -> Maybe [String]
stripArgs word = foldr g (Just []) word
    where f sym res = if null $ args sym then Just (name sym:res) else Nothing
          g sym res = branchJust res $ f sym

-- | Helper function to extract the aliased symbol from the left-hand side of a aliasing
-- rewrite rule. If the left-hand side is parameterized, then nothing is returned.
lhsToGen :: RewriteRule -> Maybe String
lhsToGen rule =
    case stripArgs $ lhs rule of
        Just [gen] -> Just gen
        _          -> Nothing

-- | Helper function to extract the alias from the right-hand side of a aliasing rewrite
-- rule. If the right-hand side is parameterized, then nothing is returned.
rhsToStr :: RewriteRule -> Maybe [String]
rhsToStr rule = stripArgs $ rhs rule

-- | Helper function to record an alias. If the generator already has an alias, then
-- nothing is returned. Otherwise, the generator gen is associated with the alias w
-- within table, and the new table is returned.
updateTable :: AliasTable -> String -> [String] -> Maybe AliasTable 
updateTable table gen def =
    if Map.member gen table
    then Nothing
    else Just $ Map.insert gen def table

-- | Helper function to record the dependencies associated with an alias. If one of the
-- symbols was not provided when the alias lookup was initialized, then nothing is
-- returned. Otherwise, dependencies are added to deps, between gen and each symbol in
-- def, and the new dependency graph is returned.
updateDeps :: AliasDeps -> String -> [String] -> Maybe AliasDeps
updateDeps deps gen def = foldr f (Just deps) def
    where f sym res = branchJust res $ \ndeps -> addEdge ndeps gen sym

-- | A function to add a new alias to an alias lookup. The original alias lookup, and the
-- aliasing rewrite rule are taken as inputs. If the rule is parameter-free, has the
-- aliased symbol on the left-hand side, and the alias on the right-hand side, then the
-- alias is recorded. Otherwise, an error is returned. If the aliased symbol is already
-- in the alias lookup, then an error is also returned. Otherwise, the new alias lookup
-- is returned. This ensures that the alias table is valid (though possibly cyclic).
--
-- Note: Alias rules must be bidirectional.
addAlias :: AliasLookup -> RewriteRule -> Either AddAliasFailure AliasLookup
addAlias (AliasLookup tab deps) rule =
    if not $ equational rule
    then Left DirectedAlias
    else case lhsToGen rule of
        Nothing  -> Left InvalidLHS
        Just gen -> case rhsToStr rule of
            Nothing  -> Left InvalidRHS
            Just def -> case updateTable tab gen def of
                Nothing   -> Left DuplicateAlias
                Just ntab -> case updateDeps deps gen def of
                    Nothing    -> Left UnexpectedSymbol
                    Just ndeps -> Right $ AliasLookup ntab ndeps
