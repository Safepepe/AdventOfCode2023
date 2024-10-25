module Day where
import Data.List (groupBy, foldl')

-- Data :
filename = "input.txt"

input :: IO [(String,[Int])]
input = readFile filename >>= return.map parse.lines
  where
  parse :: String -> (String,[Int])
  parse str = (init $ takeWhile isRow str, read $ '[':(dropWhile isRow str++"]"))
  isRow = (`elem`"#.? ")

-- Part 1 :
encode :: String -> [Int]
encode = map snd.filter ((=='#').fst).transform.groupBy (==)
  where 
  transform = map (\s -> (head s, length s))

comparing ::  ([Int] -> [Int] -> Bool) -> [Int] -> String -> Bool
comparing compareTo xs = compareTo xs.encode

lesserEq :: [Int] -> [Int] -> Bool
lesserEq xs [] = True
lesserEq [] ys = True
lesserEq xs ys = and $ zipWith (==) (init xs) (init ys)

extend :: [String] -> Char -> [String]
extend candidates '.' = map (++".") candidates
extend candidates '#' = map (++"#") candidates
extend candidates '?' = map (++"#") candidates ++ map (++".") candidates

trimWith :: [Int]  -> [String] -> [String]
trimWith xs = filter (comparing lesserEq xs)

step :: [Int] -> [String] -> Char -> [String]
step xs candidates ch = trimWith xs $ extend candidates ch

endTrim :: [Int]  -> [String] -> [String]
endTrim xs = filter (comparing (==) xs)

trimmingAlgorithm :: (String, [Int]) -> [String]
trimmingAlgorithm (str, xs) = endTrim xs $ foldl' (step xs) [""] str

-- answer :
answer1 :: (String, [Int]) -> Int
answer1 = length . trimmingAlgorithm

-- Part 2 : Brute forcing is not an option!

testData :: (String, [Int])
testData = ("???.###", [1,1,3]) -- 1 arrangement (failed)
--("???.###", [1,1,3]) -- 1 arrangement (validated)
--("????.#...#...", [4,1,1]) -- 16 arrangements (validated)
--("?###????????", [3,2,1])  -- 506250 arrangements (validated)
--(".??..??...?##.", [1,1,3]) -- 16384 arrangements (validated)
--("????.######..#####.", [1,6,5]) -- 2500 arrangements (validated)


--answer (wrong) : 
answer2 :: (String,[Int]) -> Int
answer2 (str,xs) = (aa + aar)*(a*aa + ar*aa + a*laa) + aa*(la*aa + lar*aa + la*laa)
  where
  a   = answer1 (str,xs)
  la  = answer1 ('#':str, xs)
  ar  = answer1 (str++"#", xs)
  lar = answer1 ('#':str++"#", xs)
  aa  = a*a + ar*a + a*la
  laa = answer1 ('#':str++ -- la*a + lar*a + la*la
  aar = -- a*ar + lar*a + ar*ar


