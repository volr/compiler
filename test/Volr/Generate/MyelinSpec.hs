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

import Volr.AST
import Volr.Generate.Myelin

main :: IO()
main = hspec spec

pop :: Int -> Int -> Node
pop size n = Population (fromIntegral size) if_cond_exp (show n) (n + 1)

proj :: Node -> Node -> Edge
proj n1 n2 = Projection (Static Excitatory (AllToAll (Constant 1.0))) n1 n2

testSimNetwork :: Term -> Network -> PyNNPreample -> IO ()
testSimNetwork term network preample = 
  compile Simulation term `shouldBe` (fromRight BS.empty $ fmap BS.pack $ translate network preample)

defaultPreample :: PyNNPreample
defaultPreample = PyNNPreample "nest.pyNN" ""

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
      let n1 = Population 1 if_cond_exp "0" 1
      let n2 = Population 1 if_cond_exp "2" 3
      let network = Network 4 [n1] [] [Projection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n1 n2] [n2]
      testSimNetwork (TmNet 1 1) network defaultPreample
    it "can generate parallel networks" $ do
      let n1 = Population 1 if_cond_exp "0" 1
      let n2 = Population 1 if_cond_exp "2" 3
      let n3 = Population 1 if_cond_exp "4" 5
      let n4 = Population 1 if_cond_exp "6" 7
      let p1 = Projection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n1 n2
      let p2 = Projection (Static Excitatory (AllToAll (GaussianRandom 1.0 1.0))) n3 n4
      let network = Network 8 [n1, n3] [] [p1, p2] [n2, n4]
      testSimNetwork (TmPar (TmNet 1 1) (TmNet 1 1)) network defaultPreample
--    it "can connect networks sequentially" $ do
--      let n1 = pop 1 0
--      let n2 = pop 1 2
--      let n3 = pop 1 4
--      let n4 = pop 1 6
--      let p1 = proj n1 n2
--      let p2 = proj n3 n4
--      let p3 = proj n2 n3
--      let state = Network 8 [n1] [n1, n2, n3, n4] [p1, p2, p3] [n4]
--      compile Simulation (TmSeq (TmNet 1 1) (TmNet 1 1)) `shouldBe` stateToJSON state
--    it "can connect sequential and parallel networks" $ do
--      let n1 = pop 1 0
--      let n2 = pop 2 2
--      let n3 = pop 2 4
--      let n4 = pop 2 6
--      let n5 = pop 1 8
--      let n6 = pop 1 10
--      let p1 = proj n1 n2
--      let p2 = proj n3 n4
--      let p3 = proj n5 n6
--      let p4 = proj n2 n3
--      let p5 = proj n2 n5
--      let state = Network 12 [n1] [n1, n2, n3, n4, n5, n6] [p1, p2, p3, p4, p5] [n4, n6]
--      compile Simulation (TmSeq (TmNet 1 2) (TmPar (TmNet 2 2) (TmNet 1 1))) `shouldBe` stateToJSON state
--    it "can connect parallel and sequential networks" $ do
--      let n1 = pop 2 0
--      let n2 = pop 2 2
--      let n3 = pop 1 4
--      let n4 = pop 1 6
--      let n5 = pop 1 8
--      let n6 = pop 1 10
--      let p1 = proj n1 n2
--      let p2 = proj n3 n4
--      let p3 = proj n5 n6
--      let p4 = proj n2 n5
--      let p5 = proj n4 n5
--      let state = Network 12 [n1, n3] [n1, n2, n3, n4, n5, n6] [p1, p2, p3, p4, p5] [n6]
--      compile Simulation (TmSeq (TmPar (TmNet 2 2) (TmNet 1 1)) (TmNet 1 1)) `shouldBe` stateToJSON state
--
