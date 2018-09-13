{-# LANGUAGE QuasiQuotes #-}
module Generate.Futhark where

import Control.Monad.Except
import Control.Monad.State.Lazy
import qualified Data.Map.Strict as Map
import Data.Text (unpack, pack)
import NeatInterpolation (text)
import Text.RawString.QQ
import Text.Printf

import AST

type Error = String

data NetworkState = NetworkState 
  { definitions :: [String]
  , store :: Map.Map String Int
  }

emptyNetworkState = NetworkState [] Map.empty

type CompileState = ExceptT Error (State NetworkState)

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
    Right v -> Right (v, f state)
  where 
    f (NetworkState definitions _) =
      unlines . map (\(d, i) -> "let " ++ (toName i) ++ " = " ++ d) $ zip definitions [0..(length definitions)]
  
dereference :: Term -> CompileState Int
dereference (TmNet n m) = do
  state <- get
  put $ state { definitions = (definitions state) ++ [dense n m Sigmoid 1]}
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
  indexP1 <- dereference r1
  indexP2 <- dereferenxe r2
  sl <- sizeRight s
  s1 <- sizeLeft p1
  s2 <- sizeLeft p2
  indexRep <- addDefinition $ replicate sr Sigmoid 1
  eq1 <- sequential sr s1 
  indexPar <- addDefinition $ parallel ip1 ip2
  indexSeq <- addDefinition $ sequential indexRep indexPar
  deBruijn
dereference (TmSeq (TmPar l1 l2) (TmNet r1 r2)) = do
  indexLeft <- dereference (TmPar l1 l2)
  index p1
dereference (TmPar t1 t2) = do
  i1 <- dereference t1
  i2 <- dereference t2
  put $ state { definition = definitions state ++ [parallel i1 i2]
  deBruijn

addDefinition :: String -> CompileState Int
addDefinition l = do 
  state <- get
  put $ state { definitions = definitions state ++ [l] }
  deBruijn

deBruijn :: CompileState Int
deBruijn = do
  state <- get
  return $ length $ definitions state

dense :: Int -> Int -> Activation -> Int -> String
dense i o activation seed = 
  printf "dl.layers.dense (%d, %d) %s %d" i o (show activation) seed

replicate :: Int -> Activation -> Int -> String
replicate i activation seed = 
  printf "dl.layers.replicate %d %s %d" i activation seed

sequential :: Int -> Int -> String
sequential n m = 
  printf "dl.layers.connect_layers (x%d, x%d)" n m

parallel :: Int -> Int -> String
parallel a b = 
  printf "dl.nn.connect_parallel (x%s, x%s)" a b

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
