module FileReader where

filename = "input.txt"

inputIO :: IO [String]
inputIO = readFile filename >>= return.lines
