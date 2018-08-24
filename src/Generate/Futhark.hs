module Generate.Futhark where

import Control.Monad.Except

import AST

type Error = String

type CompileState = Except Error String

compile :: Term -> Either Error String
compile _ = Left "hi"

compile' :: Term -> CompileState
compile' _ = throwError ""
