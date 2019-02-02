{-# LANGUAGE QuasiQuotes #-}
module Volr.Generate.Futhark where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Data.Maybe
import Data.Text (unpack, pack)
import NeatInterpolation (text)
import Text.RawString.QQ
import Text.Printf

import Volr.AST

type Error = String

data NetworkState = NetworkState 
  { definitions :: [String]
  , store :: Map.Map String Int
  }

emptyNetworkState = NetworkState [] Map.empty

type CompileState = ExceptT Error (State NetworkState)

data Activation
  = Identity
  | ReLU
  | Sigmoid

instance Show Activation where
  show Identity = "dl.nn.identity"
  show ReLU     = "dl.nn.relu"
  show Sigmoid  = "dl.nn.sigmoid"

data LossFunction 
  = CrossEntropy

instance Show LossFunction where
  show CrossEntropy = "dl.loss.softmax_cross_entropy_with_logits"

data FutharkProgram = FutharkProgram 
  { train :: Maybe Int
  , validation :: Maybe Int
  , batchSize :: Maybe Int
  , alpha :: Maybe Float
  , lossFunction :: LossFunction
  }

defaultFutharkProgram = FutharkProgram Nothing Nothing Nothing Nothing CrossEntropy

-- | Futhark preample
programPrefix = [r|import "lib/github.com/HnimNart/deeplearning/deep_learning"
module dl = deep_learning f64
|]

programTrainingTestingSplit :: Float -> String -> String
programTrainingTestingSplit fraction name = 
  let fractionString = pack $ show fraction
      nameString = pack name
  in  unpack [text|  let ${nameString}_l = i32.f64 (f64.i32 m * ${fractionString})
  let ${nameString} = ${nameString}_l - (${nameString}_l %% batch_size)|]

-- | Futhark main statement
programSuffix :: FutharkProgram -> String
programSuffix program = 
    let trainText = pack $ fromMaybe (programTrainingTestingSplit 0.8 "train") $ fmap show $ train program
        validationText = pack $ fromMaybe (programTrainingTestingSplit 0.2 "validation") $ fmap show $ validation program
        batchText = pack $ fromMaybe "128" $ fmap show $ batchSize program
        alphaText = pack $ fromMaybe "0.1" $ fmap show $ alpha program
        lossText = pack $ show $ lossFunction program
    in  unpack $ [text|
let main [m] (input:[m][]dl.t) (labels:[m][]dl.t) =
  let batch_size = ${batchText}
  ${trainText}
  ${validationText}
  let alpha = ${alphaText}
  let nn' = dl.train.gradient_descent nn alpha
            input[:train] labels[:train]
            batch_size ${lossText}
  let acc = dl.nn.accuracy nn' input[train:train+validation]
     labels[train:train+validation] dl.nn.softmax dl.nn.argmax
  in (acc, nn'.weights)
    |]

compile :: FutharkProgram -> Term -> Either Error String
compile program term = 
  case (compile' term) of
    Left error -> Left error
    Right (index, code) -> Right $ f index code
  where f :: Int -> String -> String 
        f index code = 
          let body = "let nn = " ++ (toName (index - 1))
              tail = programSuffix program
          in  programPrefix ++ code ++ "\n" ++ body ++ "\n" ++ tail

compile' :: Term -> Either Error (Int, String)
compile' term = 
  let (value, state) = runState (runExceptT $ dereference term) emptyNetworkState
  in   case value of
    Left error -> Left error 
    Right v -> Right (v + 1, f state)
  where 
    f (NetworkState definitions _) =
      unlines . map (\(d, i) -> "let " ++ (toName i) ++ " = " ++ d) $ zip definitions [0..(length definitions)]
  
dereference :: Term -> CompileState Int
dereference (TmNet n m) = do
  addDefinition $ denseLayer n m ReLU 1
  deBruijn
dereference (TmRef name) = do
  state <- get
  case Map.lookup name (store state) of
    Nothing -> throwError $ "Reference " ++ name ++ " not found"
    Just i -> return i
dereference (TmLet name t1 t2) = do
  index <- dereference t1
  state <- get 
  put $ state { store = Map.insert name index (store state) }
  index' <- dereference t2
  state' <- get
  put $ state' { store = Map.delete name (store state') }
  return index'
dereference (TmSeq (TmNet l1 l2) (TmPar r1 r2)) = do
  indexLeft <- dereference (TmNet l1 l2)
  indexRep <- addDefinition $ replicateLayer l2 ReLU 1
  connLeft <- addDefinition $  sequentialConnection indexLeft indexRep
  indexP1 <- dereference r1
  indexP2 <- dereference r2
  connPar <- addDefinition $ parallelConnection indexP1 indexP2
  connSeq <- addDefinition $ sequentialConnection connLeft connPar
  sizePar1 <- sizeRight r1
  sizePar2 <- sizeRight r2
  indexMerge <- addDefinition $ mergeLayer sizePar1 sizePar2
  addDefinition $ sequentialConnection connSeq indexMerge
  deBruijn
dereference (TmSeq (TmPar l1 l2) (TmNet r1 r2)) = do
  indexLeft <- dereference (TmPar l1 l2)
  size1 <- sizeRight l1
  size2 <- sizeRight l2
  indexMerge <- addDefinition $ mergeLayer size1 size2
  connMerge <- addDefinition $ sequentialConnection indexLeft indexMerge
  indexDense <- addDefinition $ denseLayer r1 r2 ReLU 1
  addDefinition $ sequentialConnection connMerge indexDense
  deBruijn
dereference (TmSeq (TmPar l1 l2) (TmPar r1 r2)) = do
  indexLTop <- dereference l1
  indexLBottom <- dereference l2
  connLeft <- addDefinition $ parallelConnection indexLTop indexLBottom
  indexRTop <- dereference r1
  indexRBottom <- dereference r2
  connRight <- addDefinition $ parallelConnection indexRTop indexRBottom
  addDefinition $ sequentialConnection connLeft connRight
dereference (TmSeq t1 t2) = do
  n1 <- dereference t1
  n2 <- dereference t2
  addDefinition $ sequentialConnection n1 n2
dereference (TmPar t1 t2) = do
  i1 <- dereference t1
  i2 <- dereference t2
  addDefinition $ parallelConnection i1 i2
  deBruijn

addDefinition :: String -> CompileState Int
addDefinition name =
  do 
    state <- get
    put $ state { definitions = definitions state ++ [name] }
    deBruijn

deBruijn :: CompileState Int
deBruijn = do
  state <- get
  return $ (length $ definitions state) - 1

denseLayer :: Int -> Int -> Activation -> Int -> String
denseLayer i o activation seed = 
  printf "dl.layers.dense (%d, %d) %s %d" i o (show activation) seed

replicateLayer :: Int -> Activation -> Int -> String
replicateLayer i activation seed = 
  printf "dl.layers.replicate %d %s %d" i (show activation) seed

mergeLayer :: Int -> Int -> String
mergeLayer i1 i2 = 
  printf "dl.layers.merge (%d, %d)" i1 i2

sequentialConnection :: Int -> Int -> String
sequentialConnection n m = 
  printf "dl.nn.connect_layers x%d x%d" n m

parallelConnection :: Int -> Int -> String
parallelConnection a b = 
  printf "dl.nn.connect_parallel x%d x%d" a b

toName :: Int -> String
toName = (++) "x" . show

sizeLeft :: Term -> CompileState Int
sizeLeft term = 
  case term of 
    TmNet m _ -> return m 
    TmSeq t1 t2 -> sizeLeft t1 
    TmPar t1 t2 -> sizeLeft t1
    _ -> throwError $ "Cannot extract size from term " ++ (show term)

sizeRight :: Term -> CompileState Int
sizeRight term = 
  case term of 
    TmNet _ m -> return m 
    TmSeq t1 t2 -> sizeRight t2
    TmPar t1 t2 -> sizeRight t2
    _ -> throwError $ "Cannot extract size from term " ++ (show term)
