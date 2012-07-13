import Control.Applicative
import System.Environment

import qualified Option as Opt
import LL

main :: IO ()
main = do
  opt <- Opt.parseIO 
  let Opt.Option (Opt.InputFile infile) (Opt.AnswerFile ansfile) = opt
  txt <- readFile infile
  let bd0 = reverse $ lines txt
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
  mvs <- filter (`elem` "LRUDWA") <$> readFile ansfile
  print =<< simulate bd mvs
