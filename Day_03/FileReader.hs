module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO Map
inputIO = readFile filename >>= return.addCoords.lines

type Position = (Int,Int)
type Map = [[(Position,Char)]]

addCoords :: [String] -> Map
addCoords  = map change . zip [0..] . map (zip [0..])
    where change :: (Int, [(Int,Char)]) -> [(Position,Char)]
          change (row, []) = []
          change (row, (column,ch):rest) = ((row,column),ch) : change (row,rest)
