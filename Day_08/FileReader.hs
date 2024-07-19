module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO ([Instruction], Network)
inputIO = readFile filename >>=  return.fst.head.parse parseInput


data Instruction = L | R
    deriving(Eq,Show)
type Node = String
type Paths = Instruction -> Node
type Branch = (Node, Paths)
type Network = [Branch]

parseNode :: Parser String
parseNode = sequence [upper,upper,upper]

parseBranch :: Parser Branch
parseBranch = do
              nodeName <- token parseNode
              token $ char '='
              char '('
              leftNode <- parseNode
              string ", "
              rightNode <- parseNode
              char ')'
              return (nodeName, \i -> if i == L then leftNode else rightNode)

parseL :: Parser Instruction
parseL = char 'L' >> return L

parseR :: Parser Instruction
parseR = char 'R' >> return R

parseInput:: Parser ([Instruction], Network)
parseInput = do
             ins <- some (parseL <|> parseR)
             network <- some $ char '\n' >> parseBranch
             return (ins,network)
