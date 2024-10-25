module Day where

type Coord = (Int,Int)
type Position = (Char,  Coord)
type TextMap = [[Char]] -- [[Position]]

-- Problem data:
filename = "input.txt"

inputMap :: IO TextMap
inputMap = readFile filename >>=  return.lines

-- Part 1 :
expandRows :: TextMap -> TextMap
expandRows [] = []
expandRows (r:rs) 
  |all (=='.') r = r:r:expandRows rs
  |otherwise     = r:expandRows rs

transpose :: TextMap -> TextMap
transpose = foldr (zipWith (:)) (repeat [])

expandMap :: TextMap -> TextMap
expandMap = transpose . expandRows . transpose . expandRows

addCoords :: [[a]] -> [[(a,Coord)]]
addCoords cs = zipWith zip cs coords
  where
  coords = zipWith zip rowCoords colCoords
  rowCoords = (replicate ncol <$> [0 .. (nrow - 1)]) 
  colCoords = (replicate nrow [0 .. (ncol-1)]) 
  ncol = length (cs !! 0)
  nrow = length cs

manhattanDistance :: (Coord, Coord) -> Int
manhattanDistance ((x0,y0), (x1,y1)) = abs(x0-x1) + abs(y0-y1)

stars :: TextMap -> [Coord]
stars = starPositions . addCoords
  where
  starPositions = concatMap (map snd . filter ((=='#').fst))

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x:xs) = map (x,) xs ++ pairs xs

--- Part 1 answer: 
answer1 :: IO Int
answer1 = inputMap >>= return.sum.map manhattanDistance.pairs.stars.expandMap

-- Part 2 : 
superExpandRows :: TextMap -> TextMap
superExpandRows [] = []
superExpandRows (r:rs) 
  |all (`elem`".*") r = ((const '*')<$> r):superExpandRows rs
  |otherwise     = r:superExpandRows rs

superExpandMap ::  TextMap -> TextMap
superExpandMap = transpose . superExpandRows . transpose . superExpandRows

superManhattanDistance :: TextMap -> (Coord, Coord) -> Int
superManhattanDistance tmap ((x0,y0), (x1,y1)) = abs(x0-x1) + abs(y0-y1) + offset
  where
  expansion = 1000000 
  offset = (expansion -1)*(cols + rows)
  cols = length . filter (=='*') . drop (min y0 y1) . take (max y0 y1) . (!!0) $  tmap
  rows = length . filter (=='*') . drop (min x0 x1) . take (max x0 x1) . map (!!0) $ tmap

--- Part 1 answer: 
answer2 :: IO Int
answer2 = do 
  tmap <- superExpandMap <$> inputMap 
  return.sum.map (superManhattanDistance tmap).pairs.stars$ tmap
