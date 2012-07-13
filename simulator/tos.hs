import System.Environment (getArgs)
import Control.Applicative

data Cell = Robot | Wall | Rock | Lambda | ClosedLift | OpenLift | Earth | Empty
type Mine = Array (Int,Int) Cell

instance Show Cell where
  show Robot = "R"
  show Wall = "#"
  show Rock = "*"
  show Lambda = "\\"
  show ClosedLift = "L"
  show OpenLift = "O"
  show Earth = "."
  show Empty = " "


main = do
  (filename:_) <- getArgs
  mine <- parseMine <$> readFile filename
  moves <- parseMoves <$> getContents
  undefined


parseMine :: String -> Mine
parseMine = unlines

parseMoves :: String -> [Move]
parseMoves = concatMap f where
  f 'L' = Left
  f 'R' = Right
  f 'U' = Up
  f 'D' = Down
  f 'W' = Wait
  f 'A' = Abort

