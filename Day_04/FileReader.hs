module FileReader where
import Parsing

filename = "input.txt"

inputIO :: IO [Card]
inputIO = readFile filename >>= return.map (fst.head.parse card).lines

type CardID = Int
type WinningNumber = Int
type CardNumber = Int
type Card = (CardID, ([WinningNumber], [CardNumber]))

card :: Parser Card
card = do 
       string "Card"
       cardId <- natural
       char ':'
       winningNumz <- some natural
       char '|'
       cardNumz <- some natural
       return (cardId, (winningNumz, cardNumz))
