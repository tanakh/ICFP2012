module Option where

import Options.Applicative
import Options.Applicative.Builder

data Option = 
  Option {
    input :: Input,
    answer :: Answer,
    verbose :: Bool
  }
  
data Input  = InputFile FilePath | Stdin
data Answer = AnswerFile FilePath | Keyboard | Auto


parseIO :: IO Option
parseIO = execParser $ info (helper <*> parse)
   ( fullDesc
   & progDesc "Lambda Lifting Simulator"
   & header "ll-simulator")

parse :: Parser Option
parse = Option 
        <$> strOption ( 
          long "input" 
          & short 'i' 
          & transform InputFile
          & value Stdin 
          & help "mine data input filename; (default) read from stdin") 
        <*> strOption ( 
          long "ans" 
          & short 'a' 
          & transform fa
          & value Auto
          & help "answer filename; 'kbd' interactive gameplay; (default) run solver")
        <*> switch ( 
          long "verbose"
          & short 'v'
          & help "be verbose")
  where
    fa str = case str of
      "kbd"  -> Keyboard
      _      -> AnswerFile str
