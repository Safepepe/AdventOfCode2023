module Day where
import FileReader  --inputIO :: IO [History]
--import Control.Monad.Reader

--type History = [Int]

-- Part One --
histDiff :: History -> History --unsafe
histDiff xs = zipWith (-) (tail xs) xs

allZeros :: History -> Bool
allZeros = and . map (==0)

step :: ([Int], History) -> ([Int],History)
step (xs,hist) = (x:xs, newHist)
    where newHist = histDiff hist
          x = last newHist

lastDiffs :: History -> [Int]
lastDiffs xs = fst $ until (\(is,hs) -> allZeros hs) step ([last xs],xs)

extrapolate :: History -> Int
extrapolate = sum . lastDiffs

answer1 :: IO Int
answer1 = inputIO >>= return.sum.map extrapolate

-- Part Two --
answer2 :: IO Int
answer2 = inputIO >>= return.sum.map extrapolate. map reverse

