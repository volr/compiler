{-# LANGUAGE TemplateHaskell #-}
module Generate.Myelin where

import Control.Lens
import Control.Monad (sequence_)
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.Aeson
import Data.ByteString.Lazy
import Myelin.SNN

import AST

data Target
  = Simulation
  | Hardware

compile :: Target -> Term -> ByteString
compile target term =
  let ((_, output), inputBlock) = runState (compile' term) initialBlockState
      block = inputBlock & outputs .~ output
      network = Network [block]
      myelinTarget = case target of
        Simulation -> Nest 0 0
	Hardware -> BrainScaleS 0 0
      task = Task myelinTarget network 0
  in encode task

compile' :: Term -> SNN ([Node], [Node]) Identity
compile' (TmNet l r) = do  
  p1 <- lifPopulation l
  p2 <- lifPopulation r
  projection (AllToAll 1.0 False) (Static Excitatory) p1 p2 
  return ([p1], [p2])
compile' (TmSeq (TmPar lt lb) r) = do
  (lt1, lt2) <- compile' lt
  (lb1, lb2) <- compile' lb
  (r1, r2) <- compile' r
  projectAll lt2 r1
  projectAll lb2 r1
  return (lt1 ++ lb1, r2)
-- compile' (TmSeq l (TmPar bl br) = do
--   (l1, l2) <- compile' l 
--   (r1, r2) <- compile' b1
--   
--   return ([l1], [r2])
-- compile' (TmPar t b) = do
--   (t1, t2) <- compile' t
--   (b1, b2) <- compile' b
--   return ([t1, b1], [t2, b2])

projectAll :: [Node] -> [Node] -> SNN () Identity
projectAll lefts rights = do
  mapM_ (\n -> mapM_ (\m -> project n m) rights) lefts

project :: Node -> Node -> SNN () Identity
project left right = projection (AllToAll 1.0 False) (Static Excitatory) left right

lifPopulation :: Int -> SNN Node Identity
lifPopulation size = do
  label <- newId
  population (show label) (fromIntegral size) if_cond_exp 
