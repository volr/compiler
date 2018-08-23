module AST where

import qualified Data.Map.Strict as Map

-- | The terms in the abstract syntax tree
data Term
  = TmNet Int Int
  | TmSeq Term Term 
  | TmPar Term Term
  | TmRef String
  | TmLet String Term Term
  deriving (Eq, Show)

-- | The types of language constructions
data Type
  = TyNetwork Int Int
  | TyInt
  deriving (Eq, Show)

-- | Type context for constants
type Context = Map.Map String Type

-- | Constant store 
type Store = Map.Map String Term