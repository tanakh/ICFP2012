import Control.Applicative
import System.Environment

import LL

main :: IO ()
main = do
  [infile, ansfile] <- getArgs
  txt <- readFile infile
  let bd0 = reverse $ lines txt
      w = maximum $ map length bd0
      bd = map (take w . (++ repeat ' ')) bd0
  mvs <- filter (`elem` "LRUDWA") <$> readFile ansfile
  print =<< simulate bd mvs
