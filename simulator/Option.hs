module Option where

import Options.Applicative
import Options.Applicative.Builder

data Option = 
  Option {
    input :: Input,
    answer :: Answer,
    replay :: Replay,
    verbose :: Bool
  }
  
data Input  = InputFile FilePath | Stdin deriving (Eq, Show)
data Answer = AnswerFile FilePath | Keyboard | Auto deriving (Eq, Show)
data Replay = ReplayFile FilePath | ReplayDefault | ReplayNothing deriving (Eq, Show)

parseIO :: IO Option
parseIO = execParser $ info (helper <*> parse)
   ( fullDesc
   & header "*** Lambda Lifting Simulator ***"
   & progDesc 
   (unlines ["auto mode :",
             "  write the Mining Robot AI, dig for lambda and save the world!",
             "interactive mode :",
             "  [h][j][k][l] or [cursor keys] for move,",
             "  [.] or [space] for wait, [backspace] for undo, [q] for abort."]))

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
        <*> strOption (
          long "replay"
          & short 'r'
          & transform fr
          & value ReplayDefault
          & help "replay filename; 'none'; (default) give default filename")
        <*> switch ( 
          long "verbose"
          & short 'v'
          & help "be verbose")
  where
    fa str = case str of
      "kbd"  -> Keyboard
      _      -> AnswerFile str
    fr str = case str of
      "none" -> ReplayNothing
      _      -> ReplayFile str
      