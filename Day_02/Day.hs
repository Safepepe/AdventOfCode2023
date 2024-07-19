module Day where
import FileReader (inputIO) -- inputIO :: IO [Game]

-- Part one --
type ID = Int
type Color = String
type Entry = (Color, Int)
type Set = [Entry] 
type Game = (ID,[Set])

maxOf :: Color -> Game -> Int
maxOf clr (i,sts) = maximum $ map snd $ filter ((== clr) . fst) $ concat sts

possible :: Game -> Bool
possible g = redCondition && greenCondition && blueCondition 
            where 
            redCondition = maxOf "red" g <= 12 
            greenCondition = maxOf "green" g <= 13 
            blueCondition = maxOf "blue" g <= 14 

possibleGames :: IO [Int]
possibleGames = inputIO >>= return.map fst.filter possible

-- Part two --
powerOf :: Game -> Int
powerOf g = product $ (maxOf <$> ["red","green","blue"]) <*> [g]

answer2 :: IO Int
answer2 = inputIO >>= return.sum.map powerOf

