{-# LANGUAGE TemplateHaskell #-}
module Volr.Generate.Myelin where

import Control.Lens
import Control.Monad (sequence_)
import Control.Monad.Identity
import Control.Monad.State.Lazy
import Data.ByteString.Lazy.Char8
import Myelin.Model
import Myelin.Neuron
import Myelin.SNN hiding (Hidden, Output, Input)
import qualified Myelin.SNN as SNN (PopulationVisibility (Input, Output, Hidden))

import Myelin.PyNN.PyNN

import Volr.AST

data Target
  = Simulation
  | Hardware
  deriving (Eq, Show)

data NetworkVisibility
  = InputOutput
  | Input
  | Output
  | Hidden
  deriving (Eq, Show)

compile :: Target -> Term -> ByteString
compile target term =
  let network = execState (compile' term InputOutput) initialNetwork
      preample = PyNNPreample "nest.pyNN" ""
  in  pack $ either Prelude.id Prelude.id $ translate network preample

compile' :: Term -> NetworkVisibility -> SNN ([Node], [Node]) Identity
compile' (TmNet l r) visibility = do
  p1 <- lifPopulation l (toInput visibility)
  p2 <- lifPopulation r (toOutput visibility)
  projection (Static Excitatory (AllToAll (GaussianRandom 1 1))) p1 p2 
  return ([p1], [p2])
compile' (TmSeq (TmPar lt lb) (TmPar rt rb)) visibility = do
  (lt1, lt2) <- compile' lt (leftVisible visibility)
  (lb1, lb2) <- compile' lb Hidden
  (rt1, rt2) <- compile' rt Hidden
  (rb1, rb2) <- compile' rb (rightVisible visibility)
  projectAll (lt2 ++ lb2) (rt1 ++ rb1)
  return (lt1 ++ lb1, rt2 ++ rb2)
compile' (TmSeq (TmPar lt lb) r) visibility = do
  (lt1, lt2) <- compile' lt (leftVisible visibility)
  (lb1, lb2) <- compile' lb Hidden
  (r1, r2) <- compile' r (rightVisible visibility)
  projectAll (lt2 ++ lb2) r1
  return (lt1 ++ lb1, r2)
compile' (TmSeq l (TmPar lt lb)) visibility = do
  (l1, l2) <- compile' l (leftVisible visibility)
  (lt1, lt2) <- compile' lt (rightVisible visibility)
  (lb1, lb2) <- compile' lb (rightVisible visibility)
  projectAll l2 (lt1 ++ lb1)
  return (l1, lt2 ++ lb2)
compile' (TmSeq l r) visibility = do
  (l1, l2) <- compile' l (leftVisible visibility)
  (r1, r2) <- compile' r (rightVisible visibility)
  projectAll l2 r1
  return (l1, r2)
compile' (TmPar t b) visibility = do
  (t1, t2) <- compile' t visibility
  (b1, b2) <- compile' b visibility
  return (t1 ++ b1, t2 ++ b2)

projectAll :: [Node] -> [Node] -> SNN () Identity
projectAll lefts rights = do
  mapM_ (\n -> mapM_ (\m -> project n m) rights) lefts

project :: Node -> Node -> SNN () Identity
project left right = projection (Static Excitatory (AllToAll (GaussianRandom 1 1))) left right

lifPopulation :: Int -> PopulationVisibility -> SNN Node Identity
lifPopulation size visibility = do
  label <- newId
  population (show label) (fromIntegral size) if_cond_exp visibility 

leftVisible :: NetworkVisibility -> NetworkVisibility
leftVisible InputOutput = Input
leftVisible Input = Input
leftVisible _ = Hidden

rightVisible :: NetworkVisibility -> NetworkVisibility
rightVisible InputOutput = Output
rightVisible Output = Output
rightVisible _ = Hidden

toInput :: NetworkVisibility -> PopulationVisibility
toInput Input = SNN.Input
toInput InputOutput = SNN.Input
toInput _ = SNN.Hidden

toOutput :: NetworkVisibility -> PopulationVisibility
toOutput Output = SNN.Output
toOutput InputOutput = SNN.Output
toOutput _ = SNN.Hidden
