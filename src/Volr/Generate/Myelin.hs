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
      preample = PyNNPreample ""
      myelinTarget = case target of
        Simulation -> Nest 0 0
        Hardware -> BrainScaleS 0 0
      task = Task myelinTarget network 50
  in  case translate task preample of
        Right out -> pack $ out
        Left error -> pack $ "Error when compiling network: " ++ error

compile' :: Term -> NetworkVisibility -> SNN ([Node], [Node]) Identity
compile' (TmNet l r) visibility = do
  p1 <- lifPopulation l (toInput visibility)
  p2 <- lifPopulation r (toOutput visibility)
  project [p1] [p2]
  return ([p1], [p2])
compile' (TmSeq (TmPar lt lb) (TmPar rt rb)) visibility = do
  (lt1, lt2) <- compile' lt (leftVisible visibility)
  (lb1, lb2) <- compile' lb (leftVisible visibility)
  (rt1, rt2) <- compile' rt (leftVisible visibility)
  (rb1, rb2) <- compile' rb (rightVisible visibility)
  project (lt2 ++ lb2) (rt1 ++ rb1)
  return (lt1 ++ lb1, rt2 ++ rb2)
compile' (TmSeq (TmPar lt lb) r) visibility = do
  (lt1, lt2) <- compile' lt (leftVisible visibility)
  (lb1, lb2) <- compile' lb (leftVisible visibility)
  (r1, r2) <- compile' r (rightVisible visibility)
  project (lt2 ++ lb2) r1
  return (lt1 ++ lb1, r2)
compile' (TmSeq l (TmPar lt lb)) visibility = do
  (l1, l2) <- compile' l (leftVisible visibility)
  (lt1, lt2) <- compile' lt (rightVisible visibility)
  (lb1, lb2) <- compile' lb (rightVisible visibility)
  project l2 (lt1 ++ lb1)
  return (l1, lt2 ++ lb2)
compile' (TmSeq l r) visibility = do
  (l1, l2) <- compile' l (leftVisible visibility)
  (r1, r2) <- compileRight r (rightVisible visibility)
  project l2 r2
  return (l1, r2)
compile' (TmPar t b) visibility = do
  (t1, t2) <- compile' t visibility
  (b1, b2) <- compile' b visibility
  return (t1 ++ b1, t2 ++ b2)

compileRight :: Term -> NetworkVisibility -> SNN ([Node], [Node]) Identity
compileRight (TmNet l r) v = do
  r' <- lifPopulation r (toOutput v)
  return ([r'], [r'])
compileRight (TmSeq l r) v = do
  (ll, lr) <- compileRight l v
  (rl, rr) <- compile' r v
  project lr rl
  return (ll, rr)
compileRight (TmPar t b) v = do
  (tl, tr) <- compileRight t v
  (bl, br) <- compileRight b v
  return (tl ++ bl, tr ++ br)

project :: [Node] -> [Node] -> SNN () Identity
project left right = projection (Static Excitatory (AllToAll (BiasGenerator (Constant 0)) (WeightGenerator (GaussianRandom 1 1)))) left right

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
