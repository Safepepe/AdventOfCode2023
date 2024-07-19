module Day where
import FileReader (inputIO)
import Data.Char (isDigit)
import Data.List (nub)
import Control.Monad.Reader

type Row = Int
type Column = Int
type Position = (Row,Column)
type Map a = Reader [[(Position, Char)]] a

--Part one --
symbols :: String 
symbols = "*=-%&@$/+#"

inside :: Position -> Map Bool
inside (r,c) = 
       do 
       mp <- ask
       return $ (r>=0) && (c>=0) && (r<length mp) && (c < (length $ head mp))

lookUp :: Position -> Map (Maybe Char)
lookUp (r,c) =
        do 
        valid <- inside (r,c) 
        if not valid then return Nothing 
        else ask >>= (\mp -> return $ Just $ snd $ (mp!!r)!!c)

holdsDigit :: Position -> Map Bool 
holdsDigit p = 
        do 
        maybeChar <- lookUp p
        case maybeChar of
          Nothing -> return False
          (Just ch) -> return $ isDigit ch

adjecentsTo :: Position -> Map [(Position,Char)]
adjecentsTo (r,c) =  
        do 
        let around k = take (min 3 (k+2)) . drop (max 0 (k-1))
        rows <- around r <$> ask
        return $  filter (\((x,y),ch)->x/=r||y/=c) $ concatMap (around c) rows

keyPositions :: Map [Position]
keyPositions = 
        do  
        symbPostns <- (map fst . concatMap (filter ((`elem`symbols).snd))) <$> ask 
        neighborhoods <- (map $ map fst) <$> (mapM adjecentsTo symbPostns)
        smallNeighborhoods <- mapM (filterM holdsDigit) neighborhoods
        return $ concat smallNeighborhoods

expandNum :: Position -> Map (Maybe (Position, String))
expandNum (r,c) = 
        do
        validPos <- inside (r,c)
        if not validPos
        then return Nothing
        else do
             notNumber <- not <$> holdsDigit (r,c) 
             if notNumber 
             then return Nothing
             else do 
                  digitToMyLeft <- holdsDigit (r,c-1)
                  if digitToMyLeft then expandNum (r,c-1)
                  else do
                       mp <- ask 
                       let str = map snd $ takeWhile (isDigit.snd) $ drop c (mp!!r)
                       return $ Just ((r,c), str)

partNumbers :: Map [(Position,String)]
partNumbers = 
        do 
        maybeParts <- keyPositions >>= mapM expandNum
        let Just parts = sequence $ filter (/= Nothing) maybeParts
        return $ nub parts

answer1 :: IO Int
answer1 = inputIO >>= return.sum.map (read.snd).runReader partNumbers

--Part two -- 
--gear = any '*' adjecent to exactly two partNumbers

nearPart :: Position -> (Position, String) -> Map Bool
nearPart (r0,c0) ((r1,c1),str) =
        do 
        neighborhoodPositions <- map fst <$> adjecentsTo  (r0,c0)
        let partPositions = zip (repeat r1) $ (c1+)<$>[0 .. length str - 1]
        return $ or $ (`elem`neighborhoodPositions) <$> partPositions

pairGearParts :: [Position] -> [(Position,String)] -> Map [(Position,[(Position,String)])]
pairGearParts gearPositions parts = 
        do 
        surroundingParts <- sequence.sequence(filterM<$>nearPart<$>gearPositions) $ parts
        return $ zip gearPositions surroundingParts

gears :: Map [(Position, [Int])]
gears = do 
        potentialGears <- concatMap (map fst.filter ((=='*').snd)) <$> ask
        parts <- partNumbers 
        pairings <- pairGearParts potentialGears parts
        let gs = filter ((==2).length.snd) pairings
        return $ map (\(x,y) -> (x, (read.snd)<$>y)) gs 

answer2 :: IO Int
answer2 = inputIO >>= return.sum.map product.map snd.runReader gears
