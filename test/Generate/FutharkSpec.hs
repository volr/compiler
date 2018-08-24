{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE OverloadedStrings #-}
module Generate.FutharkSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Text.RawString.QQ

import Test.Hspec

import AST
import Generate.Futhark

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The Futhark program generator" $ do
    it "can generate a simple NN program" $ do
      let f = FutharkProgram { train = 1, validation = 1, batchSize = 1, alpha = 0.1, lossFunction = CrossEntropy}
      let o = compile f (TmNet 1 2)
      o `shouldBe` Right simpleProgram
    where simpleProgram = [r|import "../lib/deep_learning"
module dl = deep_learning f32

let nn = dl.layers.dense (1, 2) dl.nn.sigmoid 1
let main [m] (input:[m][]dl.t) (labels:[m][]dl.t) =
  let train = 1
  let validation = 1
  let batch_size = 1
  let alpha = 0.1
  let nn' = dl.train.gradient_descent nn alpha
            input[:train] labels[:train]
            batch_size dl.loss.softmax_cross_entropy_with_logits
  in dl.nn.accuracy nn' input[train:train+validation]
     labels[train:train+validation] dl.nn.softmax dl.nn.argmax
|]