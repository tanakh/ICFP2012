import Move
import Cell

import System.Environment (getArgs)
import Control.Applicative
import Data.Array
import Data.List

type Mine = Array (Int,Int) Cell

main = do
  (filename:_) <- getArgs
  mine <- parseMine <$> readFile filename
  moves <- parseMoves <$> getContents
  print (mine,moves)

parseMine :: String -> Mine
parseMine = f . map readCells . reverse . lines where
  f x = let h = length x
            w = maximum (map length x)
        in accumArray (flip const) Empty ((1,1),(h,w))
                      [((i,j),c) | (i,y) <- zip [1..] x, (j,c) <- zip [1..] y]

parseMoves :: String -> [Move]
parseMoves = map (read . singleton) . filter (`elem` "LRUDWA") where
  singleton a = [a]
