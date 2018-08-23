module Evaluator where

import Control.Applicative
import Control.Monad.Except
import Control.Monad.State.Lazy

import Data.Either
import qualified Data.Map.Strict as Map

import AST

type Error = String
data TermState = TermState { types :: Context, store :: Store }
type EvalState = ExceptT Error (State TermState)

emptyState = TermState Map.empty Map.empty

eval :: Term -> Either Error Term
eval term = evalState (runExceptT $ eval' term) emptyState

eval' :: Term -> EvalState Term
eval' term =
  case term of
    TmNet n m -> return $ TmNet n m
    TmSeq t1 t2 -> do
        t1' <- eval' t1
        t2' <- eval' t2
        return $ TmSeq t1' t2'
    TmPar t1 t2 -> do
        t1' <- eval' t1
        t2' <- eval' t2
        return $ TmPar t1' t2'
    TmRef n -> do
        state <- get
        case store state Map.!? n of
          Nothing -> throwError $ "Could not find reference of name " ++ n
          Just m -> return m
    TmLet name t1 t2 -> do
        state <- get
        t1' <- eval' t1
        put $ state { store = Map.insert name t1' (store state) }
        t2' <- eval' t2
        put $ state
        return t2'

-- | Tests whether a given term is a value
isVal :: Term -> Bool
isVal (TmNet _ _) = True
isVal _ = False

typeOf :: Term -> EvalState Type
typeOf term = 
  case term of
    TmNet n m -> return $ TyNetwork n m
    TmSeq t1 t2 -> do
      left <- sizeLeft t1
      right <- sizeRight t2
      return $ TyNetwork left right
    TmPar t1 t2 -> do
      left1 <- sizeLeft t1 
      left2 <- sizeLeft t2
      right1 <- sizeRight t1
      right2 <- sizeRight t2
      return $ TyNetwork (left1 + left2) (right1 + right2)
    TmLet name t1 t2 -> do
      state <- get
      t1' <- eval' t1
      let innerState = state { store = Map.insert name t1' (store state) }
      evalState (return $ typeOf t2) innerState

sizeLeft :: Term -> EvalState Int
sizeLeft term = 
  case term of 
    TmNet _ m -> return m 
    TmSeq t1 t2 -> sizeLeft t1 
    TmPar t1 t2 -> sizeLeft t1
    _ -> throwError $ "Cannot extract size from term " ++ (show term)

sizeRight :: Term -> EvalState Int
sizeRight term = 
  case term of 
    TmNet _ m -> return m 
    TmSeq t1 t2 -> sizeRight t2
    TmPar t1 t2 -> sizeRight t2
    _ -> throwError $ "Cannot extract size from term " ++ (show term)

    