module Volr.ParserSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Map.Strict as Map

import Test.Hspec

import Volr.AST
import Volr.Parser

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The evaluator" $ do
    it "can parse a simple network" $ do
      parse "Net 1 1" `shouldBe` (Right (TmNet 1 1))
    it "can parse a nested network" $ do
      parse "Seq (Net 1 1) (Net 1 1)" `shouldBe` (Right (TmSeq (TmNet 1 1) (TmNet 1 1)))
    it "can parse a nested parallel network" $ do
      let e = "Par (Seq (Net 1 2) (Net 2 1)) (Par (Seq (Net 1 1) (Net 1 1)) (Net 1 1))"
      parse e `shouldBe` (Right (TmPar (TmSeq (TmNet 1 2) (TmNet 2 1)) (TmPar (TmSeq (TmNet 1 1) (TmNet 1 1)) (TmNet 1 1))))
    it "can parse a let binding with a reference" $ do
      let e = "Let x = Net 1 1 in Ref x"
      parse e `shouldBe` Right (TmNet 1 1)
    it "can fail to parse an invalid network" $ do
      let e = "Seq (Net 1 2) (Net 1 1)"
      parse e `shouldSatisfy` isLeft 
