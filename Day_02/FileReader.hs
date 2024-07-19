module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO [Game]
inputIO = readFile filename >>= return.map (fst.head.parse gameParser).lines

type ID = Int
type Color = String
type Entry = (Color, Int)
type Set = [Entry] 
type Game = (ID,[Set])

colorParser :: Parser Entry
colorParser = do 
              n <- token nat
              colrz <- foldr1 (<|>) $ string <$> ["red","blue","green"]
              char ',' <|> pure ','
              return (colrz , n)

setParser :: Parser Set
setParser = do 
            allColors <- some colorParser
            char ';' <|> pure ';'
            return allColors

gameParser :: Parser Game
gameParser = do 
             string "Game"
             idNum <- token nat
             char ':'
             sets <- many setParser -- problem! stack of manies causes infitinite loop
             return (idNum, sets)

