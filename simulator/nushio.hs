{-# LANGUAGE TypeOperators #-}
{-# OPTIONS -Wall #-}
import Control.Monad
import Data.Word
import qualified Data.Vector as V
import           Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as R (map)
import qualified Data.ByteString.Char8 as BS
import System.IO

pt :: Int -> Int -> Pt
pt x y = Z :. x :. y

type Pt = (Z :. Int :. Int)
type Mine = Array D (Z :. Int :. Int) Char

readMine :: IO [BS.ByteString]
readMine = do
  eof <- isEOF
  if eof 
    then return []
    else do
      str <- BS.getLine
      xs <- readMine
      return $ str : xs

parseMine :: [BS.ByteString] -> Mine
parseMine xs = fromFunction (pt width height) f
  where 
    width = (maximum $ map BS.length xs) + 2
    height = (length $ xs) + 2
    linev = V.fromList $ [] ++ xs ++ []
    f (Z :. x :. y) 
      | x == 0 || y == 0 ||
        x == width-1 ||
        y == height-1      = '#'
      | otherwise          = let l = (linev V.! (y-1)) in
        if x <= BS.length l then BS.index l (x-1) else ' '

printMine :: Mine -> IO ()
printMine mine = do
  forM (reverse [0..height-1]) $ \y -> do
    forM [0..width-1] $ \x -> do
      putChar $ index mine (pt x y)
    putChar '\n'
  return ()
  where
    (Z :. width :. height) = extent mine

main :: IO ()      
main = do
  mine <- fmap (parseMine . reverse)  readMine
  printMine mine
  
  
