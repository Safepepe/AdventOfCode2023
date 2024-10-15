module Day where
import Data.List (unfoldr)
--import GHC.Exts (sortWith)
--import Control.Monad.Reader

type Coord = (Int,Int)
type Position = (Char,  Coord)
type TextMap = [[Char]] -- [[Position]]

-- Problem data:
filename = "input.txt"

addCoords :: [[a]] -> [[(a,Coord)]]
addCoords cs = zipWith zip cs coords
  where
  coords = zipWith zip rowCoords colCoords
  rowCoords = (replicate ncol <$> [0 .. (nrow - 1)]) 
  colCoords = (replicate nrow [0 .. (ncol-1)]) 
  ncol = length (cs !! 0)
  nrow = length cs

inputIO :: IO TextMap
inputIO = readFile filename >>= return.lines

-- Program components:

type State = (Position, Direction)
data Direction = North | South | East | West
  deriving(Eq, Show)

nextPos :: Position -> Direction -> [[Position]]-> Maybe Position
nextPos (ch,(row,col)) dir posMap 
    -- leaving the map returns Nothing
  |row <= 0                     && dir == North = Nothing 
  |row >= length posMap - 1      && dir == South = Nothing 
  |col <= 0                     && dir == West  = Nothing
  |col >= length (posMap!!0) - 1 && dir == East  = Nothing
  -- staying in the map returns the next position
  |dir == North     = Just $ (posMap!!(row-1))!!col
  |dir == South     = Just $ (posMap!!(row+1))!!col
  |dir == East      = Just $ (posMap!!row)!!(col+1)
  |dir == West      = Just $ (posMap!!row)!!(col-1)

nextDir :: Position -> Direction -> Maybe Direction
nextDir (ch,_) dir
  |dir == North && ch == '|' = Just North
  |dir == North && ch == '7' = Just West
  |dir == North && ch == 'F' = Just East
  |dir == South && ch == '|' = Just South
  |dir == South && ch == 'J' = Just West
  |dir == South && ch == 'L' = Just East
  |dir == East  && ch == 'J' = Just North
  |dir == East  && ch == '7' = Just South
  |dir == East  && ch == '-' = Just East
  |dir == West  && ch == 'L' = Just North
  |dir == West  && ch == 'F' = Just South
  |dir == West  && ch == '-' = Just West
  |otherwise                = Nothing

start :: Direction -> [[Position]] -> State
start dir posMap = (head . concatMap (filter ((=='S').fst)) $ posMap, dir)

next :: (State, [[Position]]) -> Maybe (State, (State,[[Position]]))
next ((pos,dir),posMap) = do
  newPos <- nextPos pos dir posMap
  newDir <- nextDir newPos dir 
  return ((newPos, newDir), ((newPos,newDir),posMap))

loopThrough :: Direction -> TextMap ->  IO [State]
loopThrough dir txtMap = do 
  let posMap = addCoords txtMap
      state0 = start dir posMap 
      pipeLoop = unfoldr next (state0, posMap)
  return $ state0:pipeLoop

------- PART 1 ANSWER ----- 
answer1 :: IO Int
answer1 = do 
        txtMap <- inputIO 
        loop <- loopThrough East txtMap 
        return.(`div` 2).length $ loop
---------------------------

data Mark = Inside | Outside | Pipe | Unknown
  deriving(Eq,Show)
type MarkedTextMap = [[(Position, Mark)]]
type Loop = [Position]

-- From a given position traverse the map diagonally (upwards and to the right).
diagonal :: [[a]] -> Coord -> [a]
diagonal mp coord = unfoldr upwardRight (coord, mp)
  where
  upwardRight ((row,col),mp0)
    |col < 0 || col >= length (mp0!!0) = Nothing 
    |row < 0 || row >= length mp0      = Nothing
    |otherwise                         = Just (mp0!!row!!col, ((row-1,col+1), mp0))

traverseWith :: Loop -> [(Position,Mark)] -> [(Position,Mark)]
traverseWith lp = snd . foldr (mark lp) (Outside,[])
  where
  mark :: Loop -> (Position,Mark) -> (Mark,[(Position,Mark)]) -> (Mark,[(Position,Mark)])
  mark lp0 (pos,_) (lastMark,accum) 
    |pos `elem` lp0 && fst pos `elem` "JF" = (lastMark, (pos,Pipe):accum)
    |pos `elem` lp0 && lastMark == Inside  = (Outside, (pos,Pipe):accum)
    |pos `elem` lp0 && lastMark == Outside = (Inside, (pos,Pipe):accum)
    |not (pos `elem` lp0)                  = (lastMark, (pos,lastMark):accum)

initDiagonalCoords :: [[a]] -> [Coord]
initDiagonalCoords txtMp = firstHalf ++ secondHalf
  where
  firstHalf = zip [0 .. length txtMp -1] (repeat 0) 
  secondHalf = zip (repeat (length txtMp -1)) [1 .. length (last txtMp) - 1]

-- data of part 2:
markedMap0 :: TextMap -> MarkedTextMap
markedMap0 = map (flip zip (repeat Unknown)) . addCoords 

answer2 :: Mark -> TextMap -> IO ()
answer2 mrk txtMap = do
  lp <-  map fst <$> loopThrough East txtMap
  mrkMap <- return $ markedMap0 txtMap
  let diagonals = diagonal mrkMap <$> initDiagonalCoords mrkMap --half tested (seems ok)
      markedDiagonals = map (traverseWith lp) diagonals
      markedPos = concatMap (filter ((== mrk).snd)) markedDiagonals
  print.length$ markedPos 
