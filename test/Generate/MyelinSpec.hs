{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.MyelinSpec (main, spec) where

import Control.Monad.Except
import Data.Aeson
import Data.ByteString.Lazy
import Myelin.SNN

import Test.Hspec

import AST
import Generate.Myelin

main :: IO()
main = hspec spec

stateToJSON :: BlockState -> ByteString
stateToJSON s = encode $ Task (Nest 0 0) (Network [s]) 0

pop :: Int -> Int -> Node
pop size n = Population (fromIntegral size) if_cond_exp (show n) (n + 1) True

proj :: Node -> Node -> Edge
proj n1 n2 = Projection (AllToAll 1.0 False) (Static Excitatory) n1 n2

spec :: Spec
spec = do
  describe "The SNN program generator" $ do
    it "can generate a simple SNN program" $ do
      let n1 = Population 1 if_cond_exp "0" 1 True
      let n2 = Population 1 if_cond_exp "2" 3 True
      let state = BlockState 4 [n1] [n1, n2] [Projection (AllToAll 1.0 False) (Static Excitatory) n1 n2] [n2]
      compile Simulation (TmNet 1 1) `shouldBe` stateToJSON state
    it "can generate parallel networks" $ do
      let n1 = Population 1 if_cond_exp "0" 1 True
      let n2 = Population 1 if_cond_exp "2" 3 True
      let n3 = Population 1 if_cond_exp "4" 5 True
      let n4 = Population 1 if_cond_exp "6" 7 True
      let p1 = Projection (AllToAll 1.0 False) (Static Excitatory) n1 n2
      let p2 = Projection (AllToAll 1.0 False) (Static Excitatory) n3 n4
      let state = BlockState 8 [n1, n3] [n1, n2, n3, n4] [p1, p2] [n2, n4]
      compile Simulation (TmPar (TmNet 1 1) (TmNet 1 1)) `shouldBe` stateToJSON state
    it "can connect networks sequentially" $ do
      let n1 = pop 1 0
      let n2 = pop 1 2
      let n3 = pop 1 4
      let n4 = pop 1 6
      let p1 = proj n1 n2
      let p2 = proj n3 n4
      let p3 = proj n2 n3
      let state = BlockState 8 [n1] [n1, n2, n3, n4] [p1, p2, p3] [n4]
      compile Simulation (TmSeq (TmNet 1 1) (TmNet 1 1)) `shouldBe` stateToJSON state
