module Day where
import FileReader (inputIO)
import Control.Monad.Reader

type CardID = Int
type WinningNumber = Int
type CardNumber = Int
type Card = (CardID, ([WinningNumber], [CardNumber]))

-- Part one --
matchingNumbers :: Card -> [CardNumber]
matchingNumbers (i,(wns,cns)) = filter (`elem`wns) cns

points :: [a] -> Int
points [] = 0
points xs = 2^(length xs - 1)

answer1 :: IO Int
answer1 = inputIO >>= return.sum.map (points.matchingNumbers)

--Part two --

type Deck = Reader [Card] 

numOfCopies :: Card -> Int
numOfCopies = length.matchingNumbers


expandCard :: Card -> Deck [Card]
expandCard crd = 
         do 
         let idPstns = (+(fst crd - 1)) <$> [1 .. numOfCopies crd]
             nextCardz = sequence $ (flip (!!)) <$> idPstns
         nextTier <- nextCardz <$> ask
         allCards <- concat <$> mapM expandCard nextTier
         return (crd:allCards)

totalAmountOfCards :: Deck Int
totalAmountOfCards = do 
             crds <- ask
             expandedSet <- concat <$> mapM expandCard crds
             return $ length expandedSet 
