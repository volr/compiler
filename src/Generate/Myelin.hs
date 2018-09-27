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
  let ((input, output), rawBlock) = runState (compile' term) initialBlockState
      block = rawBlock & outputs .~ output & inputs .~ input
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
compile' (TmSeq (TmPar lt lb) (TmPar rt rb)) = do
  (lt1, lt2) <- compile' lt
  (lb1, lb2) <- compile' lb
  (rt1, rt2) <- compile' rt
  (rb1, rb2) <- compile' rb
  projectAll (lt2 ++ lb2) (rt1 ++ rb1)
  return (lt1 ++ lb1, rt2 ++ rb2)
compile' (TmSeq (TmPar lt lb) r) = do
  (lt1, lt2) <- compile' lt
  (lb1, lb2) <- compile' lb
  (r1, r2) <- compile' r
  projectAll (lt2 ++ lb2) r1
  return (lt1 ++ lb1, r2)
compile' (TmSeq l (TmPar lt lb)) = do
  (l1, l2) <- compile' l 
  (lt1, lt2) <- compile' lt
  (lb1, lb2) <- compile' lb
  projectAll l2 (lt1 ++ lb1)
  return (l1, lt2 ++ lb2)
compile' (TmSeq l r) = do
  (l1, l2) <- compile' l
  (r1, r2) <- compile' r
  projectAll l2 r1
  return (l1, r2)
compile' (TmPar t b) = do
  (t1, t2) <- compile' t
  (b1, b2) <- compile' b
  return (t1 ++ b1, t2 ++ b2)

projectAll :: [Node] -> [Node] -> SNN () Identity
projectAll lefts rights = do
  mapM_ (\n -> mapM_ (\m -> project n m) rights) lefts

project :: Node -> Node -> SNN () Identity
project left right = projection (AllToAll 1.0 False) (Static Excitatory) left right

lifPopulation :: Int -> SNN Node Identity
lifPopulation size = do
  label <- newId
  population (show label) (fromIntegral size) if_cond_exp 
