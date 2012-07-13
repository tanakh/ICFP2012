import Control.Applicative
import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Storable as U
import qualified Data.Vector.Storable.Mutable as UM
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import System.Environment

import LL

main :: IO ()
main = do
  [infile, ansfile] <- getArgs
  txt <- readFile infile
  let bd0 = reverse $ lines txt
      w = maximum $ map length bd0
      bdl = map (take w . (++ repeat ' ')) bd0
  bd <- V.thaw . V.fromList =<< mapM (U.thaw . U.fromList) bdl
  mvs <- filter (`elem` "LRUDWA") <$> readFile ansfile
  result <- simulate bd mvs
  print result
  showBoard bd
