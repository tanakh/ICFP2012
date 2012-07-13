module Cell
  ( Cell (..)
  , showCell, showCells
  , readCell, readCells
  ) where

data Cell = Robot | Wall | Rock | Lambda | ClosedLift | OpenLift | Earth | Empty
  deriving (Show, Read)

showCell Robot = "R"
showCell Wall = "#"
showCell Rock = "*"
showCell Lambda = "\\"
showCell ClosedLift = "L"
showCell OpenLift = "O"
showCell Earth = "."
showCell Empty = " "

showCells = concatMap showCell

readCell [] = error "readCell"
readCell s = readCell' . head

readCell' 'R' = Robot
readCell' '#' = Wall
readCell' '*' = Rock
readCell' '\\'= Lambda
readCell' 'L' = ClosedLift
readCell' 'O' = OpenLift
readCell' '.' = Earth
readCell' ' ' = Empty
-- readCell' _ = 

readCells = map readCell'
