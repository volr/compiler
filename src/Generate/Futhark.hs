{-# LANGUAGE QuasiQuotes #-}
module Generate.Futhark where

import Control.Monad.Except
import Control.Monad.State.Lazy
import Data.Text (unpack, pack)
import NeatInterpolation (text)
import Text.RawString.QQ
import Text.Printf

import AST

type Error = String

type CompileState = Except Error

data Activation
  = Identity
  | Sigmoid

instance Show Activation where
  show Identity = "dl.nn.identity"
  show Sigmoid  = "dl.nn.sigmoid"

data LossFunction 
  = CrossEntropy

instance Show LossFunction where
  show CrossEntropy = "dl.loss.softmax_cross_entropy_with_logits"

data FutharkProgram = FutharkProgram 
  { train :: Int
  , validation :: Int
  , batchSize :: Int
  , alpha :: Float
  , lossFunction :: LossFunction
  }

-- | Futhark preample
programPrefix = [r|import "../lib/deep_learning"
module dl = deep_learning f32
|]

-- | Futhark main statement
programSuffix :: FutharkProgram -> String
programSuffix program = 
    let trainText = pack $ show $ train program
        validationText = pack $ show $ validation program
        batchText = pack $ show $ batchSize program
        alphaText = pack $ show $ alpha program
        lossText = pack $ show $ lossFunction program
    in  unpack $ [text|
let main [m] (input:[m][]dl.t) (labels:[m][]dl.t) =
  let train = ${trainText}
  let validation = ${validationText}
  let batch_size = ${batchText}
  let alpha = ${alphaText}
  let nn' = dl.train.gradient_descent nn alpha
            input[:train] labels[:train]
            batch_size ${lossText}
  in dl.nn.accuracy nn' input[train:train+validation]
     labels[train:train+validation] dl.nn.softmax dl.nn.argmax
    |]

dense :: Int -> Int -> Activation -> Int -> String
dense i o activation seed = 
  printf "dl.layers.dense (%d, %d) %s %d" i o (show activation) seed

compile :: FutharkProgram -> Term -> Either Error String
compile program term = fmap f . runExcept $ compile' term
  where f :: String -> String 
        f code = programPrefix ++ "\nlet nn = " ++ code ++ "\n" ++ (programSuffix program)

compile' :: Term -> CompileState String
compile' (TmNet n m) = return $ dense n m Sigmoid 1
compile' _ = throwError ""
