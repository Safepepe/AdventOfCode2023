module Day where
import NaturalNumberz
import FileReader (inputIO)
import Control.Monad.Reader
import GHC.Exts (sortWith)

type OldSeed = Int
type OldSource = Int
type OldDestination = Int
type OldRange = Int
type OldMapping = ((String,String), [((Destination,Source), OldRange)])
type OldAlmanac = ([Seed], [OldMapping]) --All mappings are in composable order already

--Part two --
type Seed = Natural
type ID = Natural
type Destination = Natural
type Source = Natural
type Range = Natural
type Mapping = [((Destination, Source), Range)]
type NewAlmanac = ([(Seed,Range)], [Mapping]) --All mappings are in composable order already

--help functions
deJustList :: [Maybe a] -> [a]
deJustList = foldr clean []
    where clean Nothing xs = xs
          clean (Just x) xs = x:xs


--Step 1 : treating input
almanac :: IO NewAlmanac
almanac = do (sds, oldMps) <- inputIO
             return (seedRanges sds, toNewMapping.snd<$>oldMps)
    where
    seedRanges (s1:r1:sds) = (Natural s1,Natural r1):(seedRanges sds) 
    seedRanges [] = []

toNewMapping :: [((OldDestination,OldSource),OldRange)] -> [((Destination,Source),Range)]
toNewMapping desc = addTop $ fillGaps $ sortWith (snd.fst) desc
        where 
        addTop xs = let s0 = snd.fst.head $ xs in ((s0-Natural 1,s0-Natural 1), Minf):xs
        fillGaps [] = []
        fillGaps [((d0,s0),r0)] = [((Natural d0,Natural s0),Natural r0), ((Natural $ s0+r0,Natural $ s0+r0),Pinf)]
        fillGaps (((d0,s0),r0):((d1,s1),r1):rest) 
            |s0+r0-1<s1-1 = ((Natural d0,Natural s0),Natural r0):((Natural $ s0+r0,Natural $ s0+r0),Natural (s1-s0-r0)):fillGaps (((d1,s1),r1):rest)
            |otherwise    = ((Natural d0,Natural s0),Natural r0):fillGaps (((d1,s1),r1):rest)

--Step 2: composing two mappings: 

composeRanges :: ((Destination,Source),Range) -> ((Destination,Source),Range) -> Maybe ((Destination,Source),Range)
composeRanges ((d0,s0),r0) ((d1,s1),r1) = 
    let leftEnd = max (min d0 (d0+r0-1)) (min s1 (s1+r1-1))
        rightEnd = min (max d0 (d0+r0-1)) (max s1 (s1+r1-1))
    in if leftEnd > rightEnd 
       then Nothing 
       else let leftSrc  = leftEnd-d0+s0 
                rightSrc = rightEnd-d0+s0 
                leftDest = d0+d1+leftSrc-s0-s1 
                rightDest= d0+d1+rightSrc-s0-s1 
                newRange = rightSrc - leftSrc + Natural 1
            in if leftSrc == Minf then Just ((rightDest,rightSrc),-newRange)
               else Just ((leftDest,leftSrc),newRange)

composeMaps :: Mapping -> Mapping -> Mapping
composeMaps mp1 mp2 = deJustList  $ composeRanges <$> mp1 <*> mp2

-- Step 3 : Apply mapping to a range

applyRange :: ((Destination,Source),Range) -> (ID, Range) -> Maybe (ID, Range)
applyRange ((d0,s0),r0) (i,r) = let 
                                leftEnd  = max (min s0 (s0+r0-Natural 1)) (min i (i+r-Natural 1))
                                rightEnd = min (max s0 (s0+r0-Natural 1)) (max i (i+r-Natural 1))
                                range    = rightEnd - leftEnd + Natural 1 
                                in if leftEnd > rightEnd then Nothing 
                                   else if leftEnd == Minf 
                                        then Just (d0 + rightEnd - s0, -range) 
                                        else Just (d0 + leftEnd - s0, range)

applyMapping :: Mapping -> (ID,Range) -> [(ID,Range)]
applyMapping mp x =  deJustList $ sequence (applyRange <$> mp) $ x 

answer2 :: IO (ID,Range)--[(ID,Range)]
answer2 = do 
          (sdRngs, mappings) <- almanac
          let seedToLocation = foldr1 composeMaps mappings
          return $ head $ sortWith fst $ concatMap (applyMapping seedToLocation) sdRngs

