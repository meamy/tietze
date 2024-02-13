-- | Provides an interface to format derivations in LaTeX.

module Lafont.Format.LaTeX (
    LaTeX ( .. ),
    GenMacroList,
    makeGenMacros
) where

import qualified Data.Map                    as Map
import           Lafont.Common
import           Lafont.Generators.Semantics
import           Lafont.Rewrite.Derivations
import           Lafont.Rewrite.Rules
import           Lafont.Rewrite.Summary

-----------------------------------------------------------------------------------------
-- * General Interface for Latex Components.

-- | Represents a LaTeX document component.
class LaTeX a where
    -- | Prints the component as LaTeX source code.
    toLaTeX :: a -> String

-----------------------------------------------------------------------------------------
-- * GenMacro Dictionary.

-- | Describes a newcommand definition which associates a generator symbol with its math
-- mode representation. For example, (GenMacroDef lft_SWAP01 X_0) maps the generator
-- SWAP01 to the symbol X_0 through a macro called lft_SAWP01.
data GenMacroDef = GenMacroDef String String deriving (Eq, Show)

-- | Maintains a mapping from generator names to macro definitions. Each macro definition
-- allows an end-user to decide the symbol associated with each generator in the LaTeX
-- depiction of a derivation. In terms of LaTeX source code, this component is a list of
-- newcommand statements.
newtype GenMacroList = GenMacroList (Map.Map String GenMacroDef) deriving (Eq, Show)

-- | Implementation details for makeGenMacros. Consumes the alphabet corresponding to a
-- generator dictionary. Returns the raw map encapsulated by a GenMacroDef.
makeGenMacrosImpl :: [String] -> Map.Map String GenMacroDef
makeGenMacrosImpl []         = Map.empty
makeGenMacrosImpl (gen:gens) = Map.insert gen def map
    where map = makeGenMacrosImpl gens
          idx = Map.size map
          cmd = "\\lftgen" ++ show idx
          sym = "X_{" ++ show idx ++ "}"
          def = GenMacroDef cmd sym

-- | Constructs the GenMacroList corresponding to a generator dictionary. Note that for
-- each LaTeX document, only one GenMacroList should be generated. Otherwise, the macro
-- names or corresponding math mode symbols may conflict.
makeGenMacros :: GenDict a -> GenMacroList
makeGenMacros = GenMacroList . makeGenMacrosImpl . toAlphabet

instance LaTeX GenMacroDef where
    toLaTeX (GenMacroDef cmd sym) = "\\newcommand{" ++ cmd ++ "}{" ++ sym ++ "}"

instance LaTeX GenMacroList where
    toLaTeX (GenMacroList map) = Map.foldrWithKey f "" map
        where f gen def res = let comment = "% Macro for: " ++ gen 
                              in comment ++ "\n" ++ toLaTeX def ++ "\n" ++ res
