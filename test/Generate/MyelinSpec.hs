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

spec :: Spec
spec = do
  describe "The SNN program generator" $ do
    it "can generate a simple SNN program" $ do
      let n1 = Population 1 if_cond_exp "0" 1 True
      let n2 = Population 1 if_cond_exp "2" 3 True
      let state = BlockState 4 [] [n1, n2] [Projection (AllToAll 1.0 False) (Static Excitatory) n1 n2] [n2]
      compile Simulation (TmNet 1 1) `shouldBe` (encode $ Task (Nest 0 0) (Network [state]) 0)
