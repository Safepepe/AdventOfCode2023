module Day where
import FileReader (inputIO) --inputIO :: IO [(String,Int)]
import Data.List (sortBy)

--Used for Part One
--data Card = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | J | Q | K | A
--      deriving(Eq,Show,Ord)
--Used for Part Two
data Card = J | Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten | Q | K | A
      deriving(Eq,Show,Ord)
type Hand = [Card]
type Bid = Int
data HandType = HighCard | OnePair | TwoPair | ThreeOAK | FullHouse | FourOAK | FiveOAK
    deriving(Eq,Show,Ord)

-- Part One --
toCard :: Char -> Card
toCard '2' = Two
toCard '3' = Three
toCard '4' = Four
toCard '5' = Five
toCard '6' = Six
toCard '7' = Seven
toCard '8' = Eight
toCard '9' = Nine
toCard 'T' = Ten
toCard 'J' = J
toCard 'Q' = Q
toCard 'K' = K
toCard 'A' = A

subgroups :: Eq a => [a] -> [[a]]
subgroups [] = []
subgroups (x:xs) = (x:(filter (== x) xs)):(subgroups (filter (/= x) xs))

handType :: Hand -> HandType --unsafe (assumes 5 cards in one hand)
handType xs = let subgrpLengths = length <$> subgroups xs in
             case length subgrpLengths of
                1 -> FiveOAK
                5 -> HighCard
                4 -> OnePair
                3 -> if foldr1 max subgrpLengths == 3 then ThreeOAK else TwoPair
                2 -> if foldr1 max subgrpLengths == 4 then FourOAK else FullHouse

breakTie :: Hand -> Hand -> Ordering
breakTie [] []         = EQ
breakTie (x:xs) []     = GT
breakTie [] (y:ys)     = LT
breakTie (x:xs) (y:ys) = case compare x y of
                              LT -> LT
                              GT -> GT
                              EQ -> breakTie xs ys

compareHands :: Hand -> Hand -> Ordering
compareHands hand1 hand2 
    | handType hand1 > handType hand2 = GT
    | handType hand1 < handType hand2 = LT
    | handType hand1 == handType hand2 = breakTie hand1 hand2

sortBets :: [(Hand,Bid)] -> [(Hand,Bid)]
sortBets = sortBy (\(h1,b1) (h2,b2) -> compareHands h1 h2)

answer1 :: IO Int
answer1 = do
            bets <- map (\(xs,b) -> (map toCard xs,b)) <$> inputIO
            let ranks = [1 .. length bets]
                bids = snd <$> sortBets bets
            return $ sum $ zipWith (*) ranks bids

-- Part Two --

replaceJWith :: Card -> Card -> Card
replaceJWith c1 J  = c1
replaceJWith c1 c2 = c2

replaceJsWith :: Card -> Hand -> Hand
replaceJsWith x = map (replaceJWith x)

maxWith :: (a -> a -> Ordering) -> a -> a -> a
maxWith f x y = case f x y of
                 GT -> x
                 LT -> y
                 EQ -> x

    -- simple approach (maybe too simple)
handType2 :: Hand -> HandType --unsafe (assumes 5 cards in one hand) 
handType2 xs = let singles = head <$> subgroups xs
                   potentialHands =  replaceJsWith <$> singles <*> [xs]
               in handType $ foldr1 (maxWith compareHands) potentialHands

compareHands2 :: Hand -> Hand -> Ordering
compareHands2 hand1 hand2 
    | handType2 hand1 > handType2 hand2 = GT
    | handType2 hand1 < handType2 hand2 = LT
    | handType2 hand1 == handType2 hand2 = breakTie hand1 hand2

sortBets2 :: [(Hand,Bid)] -> [(Hand,Bid)]
sortBets2 = sortBy (\(h1,b1) (h2,b2) -> compareHands2 h1 h2)


answer2 :: IO Int
answer2 = do
            bets <- map (\(xs,b) -> (map toCard xs,b)) <$> inputIO
            let ranks = [1 .. length bets]
                bids = snd <$> sortBets2 bets
            return $ sum $ zipWith (*) ranks bids
