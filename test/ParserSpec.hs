module ParserSpec (main, spec) where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Either
import qualified Data.Map.Strict as Map

import Test.Hspec

import AST
import Parser

main :: IO()
main = hspec spec

spec :: Spec
spec = do
  describe "The evaluator" $ do
    it "can parse a simple network" $ do
      parse "Net 1 1" `shouldBe` (Right (TmNet 1 1))
