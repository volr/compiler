{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Volr.Generate.FutharkSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Text.RawString.QQ

import Test.Hspec

import Volr.AST
import Volr.Generate.Futhark

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The Futhark program generator" $ do
    it "can generate a simple NN program" $ do
      let o = compile defaultFutharkProgram (TmNet 1 2)
      let simpleProgram = [r|import "lib/github.com/HnimNart/deeplearning/deep_learning"
module dl = deep_learning f64
let x0 = dl.layers.dense (1, 2) dl.nn.relu 1

let nn = x0
let main [m] (input:[m][]dl.t) (labels:[m][]dl.t) =
  let batch_size = 128
  let train_l = i32.f64 (f64.i32 m * 0.8)
  let train = train_l - (train_l %% batch_size)
  let validation_l = i32.f64 (f64.i32 m * 0.2)
  let validation = validation_l - (validation_l %% batch_size)
  let alpha = 0.1
  let nn' = dl.train.gradient_descent nn alpha
            input[:train] labels[:train]
            batch_size dl.loss.softmax_cross_entropy_with_logits
  let acc = dl.nn.accuracy nn' input[train:train+validation]
     labels[train:train+validation] dl.nn.softmax dl.nn.argmax
  in (acc, nn'.weights)
|]
      o `shouldBe` Right simpleProgram
  describe "The Futhark compiler" $ do
    it "can dereference a simple network" $ do
      let out = compile' (TmNet 1 1)
      out `shouldBe` Right(1, "let x0 = dl.layers.dense (1, 1) dl.nn.relu 1\n")
    it "can split a network" $ do
      let out = compile' $ TmPar (TmNet 1 1) (TmNet 1 1)
      out `shouldBe` Right(3, "let x0 = dl.layers.dense (1, 1) dl.nn.relu 1\n" ++
                              "let x1 = dl.layers.dense (1, 1) dl.nn.relu 1\n" ++ 
                              "let x2 = dl.nn.connect_parallel x0 x1\n"
                          )
    it "can split a network with different sizes" $ do
      let out = compile' $ TmPar (TmNet 1 1) (TmNet 1 2)
      out `shouldBe` Right(3, "let x0 = dl.layers.dense (1, 1) dl.nn.relu 1\n" ++
                              "let x1 = dl.layers.dense (1, 2) dl.nn.relu 1\n" ++
                              "let x2 = dl.nn.connect_parallel x0 x1\n"
                          )
    it "can connect a sequential and parallel network" $ do
      let out = compile' $ TmSeq (TmNet 1 1) (TmPar (TmNet 1 1) (TmNet 1 1))
      out `shouldBe` Right(9, [r|let x0 = dl.layers.dense (1, 1) dl.nn.relu 1
let x1 = dl.layers.replicate 1 dl.nn.relu 1
let x2 = dl.nn.connect_layers x0 x1
let x3 = dl.layers.dense (1, 1) dl.nn.relu 1
let x4 = dl.layers.dense (1, 1) dl.nn.relu 1
let x5 = dl.nn.connect_parallel x3 x4
let x6 = dl.nn.connect_layers x2 x5
let x7 = dl.layers.merge (1, 1)
let x8 = dl.nn.connect_layers x6 x7
|])
    it "can connect a parallel and sequential network" $ do
      let out = compile' $ TmSeq (TmPar (TmNet 1 1) (TmNet 1 2)) (TmNet 1 1)
      out `shouldBe` Right(7, [r|let x0 = dl.layers.dense (1, 1) dl.nn.relu 1
let x1 = dl.layers.dense (1, 2) dl.nn.relu 1
let x2 = dl.nn.connect_parallel x0 x1
let x3 = dl.layers.merge (1, 2)
let x4 = dl.nn.connect_layers x2 x3
let x5 = dl.layers.dense (1, 1) dl.nn.relu 1
let x6 = dl.nn.connect_layers x4 x5
|])
    it "can connect two parallel networks" $ do
      let out = compile' $ TmSeq (TmPar (TmNet 1 1) (TmNet 1 2)) (TmPar (TmNet 1 1) (TmNet 1 2))
      out `shouldBe` Right(7, [r|let x0 = dl.layers.dense (1, 1) dl.nn.relu 1
let x1 = dl.layers.dense (1, 2) dl.nn.relu 1
let x2 = dl.nn.connect_parallel x0 x1
let x3 = dl.layers.dense (1, 1) dl.nn.relu 1
let x4 = dl.layers.dense (1, 2) dl.nn.relu 1
let x5 = dl.nn.connect_parallel x3 x4
let x6 = dl.nn.connect_layers x2 x5
|])
    

