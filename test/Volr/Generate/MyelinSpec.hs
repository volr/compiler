{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Volr.Generate.MyelinSpec (main, spec) where

import Control.Monad.Except
import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Either
import Myelin.Model
import Myelin.Neuron
import qualified Myelin.SNN as SNN (PopulationVisibility (Input, Hidden, Output))
import Myelin.SNN hiding (PopulationVisibility (Input, Hidden, Output))
import Myelin.PyNN.PyNN

import Test.Hspec
import Debug.Trace (trace)

import Volr.AST
import Volr.Generate.Myelin

main :: IO()
main = hspec spec

pop :: Int -> Int -> Node
pop size n = Population (fromIntegral size) if_cond_exp (show n) (n)

proj :: Node -> Node -> Edge
proj n1 n2 = DenseProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n1 n2

testSimNetwork :: Term -> Network -> PyNNPreample -> IO ()
testSimNetwork term network preample = 
  let task = Task (Nest 0 0) network 50
      compiled = compile Simulation term
      translated = translate task preample
  in compiled `shouldBe` (fromRight BS.empty $ fmap BS.pack $ translated)

defaultPreample :: PyNNPreample
defaultPreample = PyNNPreample ""

spec :: Spec
spec = do
  describe "The input-hidden-output visibility" $ do
    it "can retain left visibility" $ do
      leftVisible InputOutput `shouldBe` Input
      leftVisible Input `shouldBe` Input
    it "can remove left visibility in output" $ do
      rightVisible Input `shouldBe` Hidden
      rightVisible Hidden `shouldBe` Hidden
    it "can retain right visibility" $ do
      rightVisible InputOutput `shouldBe` Output
      rightVisible Output `shouldBe` Output
    it "can remove output visibility in input" $ do
      leftVisible Output `shouldBe` Hidden
      leftVisible Hidden`shouldBe` Hidden
  describe "The SNN program generator" $ do
    it "can generate a simple SNN program" $ do
      let n1 = Population 1 if_cond_exp "1" 1
      let n2 = Population 2 if_cond_exp "2" 3
      let network = Network 4 [n1] [] [DenseProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n1 n2] [n2]
      testSimNetwork (TmNet 1 2) network defaultPreample
    it "can generate parallel networks" $ do
      let n1 = Population 1 if_cond_exp "1" 1
      let n2 = Population 2 if_cond_exp "3" 3
      let n3 = Population 1 if_cond_exp "5" 5
      let n4 = Population 2 if_cond_exp "7" 7
      let p1 = DenseProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n1 n2
      let p2 = DenseProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n3 n4
      let network = Network 8 [n1, n3] [] [p1, p2] [n2, n4]
      testSimNetwork (TmPar (TmNet 1 2) (TmNet 1 2)) network defaultPreample
    it "can connect networks sequentially" $ do
      let n1 = pop 1 1
      let n2 = pop 2 3
      let n3 = pop 3 5
      let p1 = proj n1 n2
      let p2 = proj n2 n3
      let network = Network 8 [n1] [n2] [p1, p2] [n3]
      testSimNetwork (TmSeq (TmNet 1 2) (TmNet 2 3)) network defaultPreample
    it "can connect sequential and parallel networks" $ do
      let n1 = pop 1 1
      let n2 = pop 2 3
      let n3 = pop 2 5
      let n4 = pop 2 7
      let n5 = pop 2 9
      let n6 = pop 1 11
      let p1 = proj n1 n2
      let p2 = proj n3 n4
      let p3 = proj n5 n6
      let p4 = ReplicateProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n2 (n3, n5)
      let network = Network 12 [n1] [n2, n3, n5] [p1, p2, p3, p4] [n4, n6]
      testSimNetwork (TmSeq (TmNet 1 2) (TmPar (TmNet 2 2) (TmNet 2 1))) network defaultPreample
    it "can connect parallel and sequential networks" $ do
      let n1 = pop 2 1
      let n2 = pop 2 3
      let n3 = pop 2 5
      let n4 = pop 1 7
      let n5 = pop 3 9
      let n6 = pop 1 11
      let p1 = proj n1 n2
      let p2 = proj n3 n4
      let p3 = proj n5 n6
      let p4 = MergeProjection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) (n2, n4) n5
      let network = Network 12 [n1, n3] [n2, n4, n5] [p1, p2, p3, p4] [n6]
      testSimNetwork (TmSeq (TmPar (TmNet 2 2) (TmNet 2 1)) (TmNet 3 1)) network defaultPreample

