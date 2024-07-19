module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO [History]
inputIO = readFile filename >>=  return.map (fst.head.(parse parseHistory)).lines

type History = [Int] 

parseHistory :: Parser History
parseHistory = some (token int)

