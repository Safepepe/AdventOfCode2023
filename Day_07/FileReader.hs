module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO [(Hand,Bid)]
inputIO = readFile filename >>= return.map (fst.head.parse bet).lines

type Hand = String
type Bid = Int

bet :: Parser (Hand,Bid)
bet = do
       hand <- some (letter <|> digit)
       space
       bid <- nat
       return (hand, bid)

