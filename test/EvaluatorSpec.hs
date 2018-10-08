module EvaluatorSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Map.Strict as Map

import Test.Hspec

import AST
import Evaluator

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The evaluator" $ do
    it "can evaluate sequential connection of two networks" $ do
      let e = TmSeq (TmNet 1 1) (TmNet 1 1)
      eval e `shouldBe` Right e
    it "can evaluate parallel connection of two networks" $ do
      let e = TmPar (TmNet 1 1) (TmNet 1 1)
      eval e `shouldBe` Right e
    it "can evaluate a let binding" $ do
      let e = TmLet "x" (TmNet 1 2) (TmSeq (TmRef "x") (TmNet 2 1))
      eval e `shouldBe` Right (TmSeq (TmNet 1 2) (TmNet 2 1))
    it "can evaluate a let binding with a reference" $ do
      let e = TmLet "x" (TmNet 1 1) (TmRef "x")
      eval e `shouldBe` Right (TmNet 1 1)
    it "can fail to evaluate a let binding with a missing reference" $ do
      eval (TmRef "x") `shouldSatisfy` (isLeft)
    it "can evaluate a let binding and discard the inner context" $ do
      let e = TmLet "x" (TmNet 1 1) (TmRef "x")
      let s = execState (runExceptT $ eval' e) emptyState
      s `shouldBe` emptyState
    it "can fail to evaluate two sequential connections with unmatched sizes" $ do
      let e = TmSeq (TmNet 1 2) (TmNet 1 1)
      eval e `shouldSatisfy` isLeft  
