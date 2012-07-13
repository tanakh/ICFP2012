module Option where

import OptParse.Applicative
import OptParse.Applicative.Builder

parse :: Parser Option
parse = Option 
        <$> (value Stdin 
             & fmap InputFile 
             (strOption ( long "input" & short 'i')))
        <*> (value Auto
             & fmap AnswerFile
             (strOption ( long "ans" & short 'a')))
            )


data Option = 
  {
    input :: Input,
    answer :: Answer
  }
  
data Input  = InputFile FilePath | Stdin
data Answer = AnswerFile FilePath | Interactive | Auto

