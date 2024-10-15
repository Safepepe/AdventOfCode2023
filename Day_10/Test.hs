module Test where
import Day

testMap :: [[Position]]
testMap = addCoords testChart

testChart :: TextMap
testChart = ["..........",
             "..........",
             "..........",
             ".F7.......",
             ".|L---7...",
             ".|....L7..",
             ".LS-7..L7.",
             "....L7..|.",
             ".....L--J.",
             ".........."]
printChart :: [[Char]] -> IO ()
printChart = putStrLn . concatMap ('\n':) 
