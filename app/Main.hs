module Main where

import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Semigroup ((<>))
import Options.Applicative

import System.IO (hPutStrLn, stderr)

import Volr.AST
import qualified Volr.Parser as VolrParser
import qualified Volr.Generate.Futhark as Futhark
import qualified Volr.Generate.Myelin as Myelin

data Configuration = Configuration
  { input :: Input
  , backend :: Backend
  }

data Backend 
  = Futhark
  | Nest
  | BrainScaleS
  deriving (Eq, Show)

data Input 
  = FileInput String
  | StdInput
  deriving (Eq, Show)

parseFileInput :: Parser Input
parseFileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "input volr file"
  )

parseBackend :: Parser Backend
parseBackend = subparser
  (  command "futhark" (info (pure Futhark) (progDesc "Generates Futhark code"))
  <> command "nest" (info (pure Nest) (progDesc "Generates NEST JSON"))
  <> metavar "BACKEND"
  ) <|> (pure Futhark)

parseInput :: Parser Input
parseInput = parseFileInput <|> (pure StdInput)

parseConfig :: Parser Configuration
parseConfig = Configuration
  <$> parseInput <*> parseBackend

readInput :: Input -> IO String
readInput (FileInput file) = readFile file
readInput StdInput = getContents

compileProgram :: Term -> Backend -> IO ()
compileProgram term backend =
  case backend of
    Futhark -> 
      case Futhark.compile Futhark.defaultFutharkProgram term of
        Left error -> hPutStrLn stderr error
        Right code -> putStrLn code
    Nest -> 
      BS.putStrLn $ Myelin.compile Myelin.Simulation term 

main :: IO ()
main = do
  (Configuration input backend) <- execParser configuration
  content <- readInput input
  case VolrParser.parse content of
    Left error -> hPutStrLn stderr $ show error
    Right term -> compileProgram term backend
  where
    configuration = info (parseConfig <**> helper)
      ( fullDesc
      <> progDesc "Parses a Volr file and evaluates the model"
      <> header "Volr - a DSL for neuroscientific machine learning models" )
