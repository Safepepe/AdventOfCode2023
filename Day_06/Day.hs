module Day where
import Data.List (unfoldr)

-- Part One --
type Time = Integer
type Distance = Integer
type Speed = Integer
type RecordDistance = Integer
type Race = (Time, RecordDistance)

time1 :: [Time]
time1 = [35, 93, 73, 66]
distance1 :: [Distance]
distance1 = [212, 2060, 1201, 1044]

input1 = zip time1 distance1

marginOfError :: [Race] -> Int
marginOfError = product . map numOfWins

numOfWins :: Race -> Int
numOfWins (t,rd) = length $ filter (>rd) $ unfoldr genDistance (t,0)

genDistance :: (Time, Speed) -> Maybe (Distance, (Time,Speed)) --unsafe
genDistance (t,s) 
    | t < 0     = Nothing
    | otherwise = Just (t*s, (t-1,s+1))

answer1 :: [Int]
answer1 = numOfWins <$> input1

-- Part Two --

time2 :: Time
time2 = 35937366
distance2 :: Distance
distance2 = 212206012011044

input2 = (time2, distance2)

numOfWins2 :: Race -> (Integer, Integer)
numOfWins2 (t,rd) = (ceiling leftValue, floor rightValue)
    where
    delta0 = fromInteger (t*t-4*rd) :: Float
    t0 = fromInteger t :: Float
    leftValue  = 0.5*(t0-sqrt(delta0))
    rightValue = 0.5*(t0+sqrt(delta0))

distance :: Speed -> Distance
distance s = s*(fromInteger time2 - s)

(l,r) = numOfWins2 input2 

answer2 = (r-1) - l +1 
