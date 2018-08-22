module AST where

import qualified Data.Map.Strict as Map

-- | The terms in the abstract syntax tree
data Term
  = TmDense Int Int
  | TmCon Term Term 
  | TmPar Term Term
  | TmRef String
  | TmLet String Term Term
  deriving (Show)

-- | The types of language constructions
data Type
  = TyNetwork Int Int
  | TyInt

type Context = Map.Map String Type

type Store = Map.Map String Term