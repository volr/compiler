module Main where


import Data.Semigroup ((<>))
import Options.Applicative

import System.IO (hPutStrLn, stderr)

import AST
import qualified Parser as VolrParser
import qualified Generate.Futhark as Futhark

data Configuration = Configuration
  { input :: Input
  }

data Backend 
  = Futhark

data Input 
  = FileInput String
  | StdInput

data Syntax
  = Simple
  | Complex

parseFileInput :: Parser Input
parseFileInput = FileInput <$> strOption
  (  long "file"
  <> short 'f'
  <> metavar "FILENAME"
  <> help "input volr file"
  )

--parseSyntaxType :: Parser Syntax
--parseSyntaxType = (simple <|> complex) <> value Simple
--  where
--    simple = flag' Simple (long "simple" <> help "Toggle Simple Volr syntax")
--    complex = flag' Complex (long "complex" <> help "Toggle Complex Volr syntax")   

parseInput :: Parser Input
parseInput = parseFileInput
  <|> pure StdInput

parseConfig :: Parser Configuration
parseConfig = Configuration
  <$> parseInput

readInput :: Input -> IO String
readInput (FileInput file) = readFile file
readInput StdInput = getContents

compileProgram :: Term -> IO ()
compileProgram term =
  let program = Futhark.FutharkProgram 1 1 1 0.1 Futhark.CrossEntropy 
  in  case Futhark.compile program term of
	Left error -> hPutStrLn stderr error
	Right code -> putStrLn code

main :: IO ()
main = do
  (Configuration input) <- execParser configuration
  content <- readInput input
  case VolrParser.parse content of
    Left error -> hPutStrLn stderr $ show error
    Right term -> compileProgram term
  where
    configuration = info (parseConfig <**> helper)
      ( fullDesc
      <> progDesc "Parses a Volr file and evaluates the model"
      <> header "Volr - a DSL for neuroscientific machine learning models" )
