module Day where
import FileReader (inputIO)
import Data.Char (isDigit)
import Parsing

-- Part One --

decodeLine :: String -> Int
decodeLine  = joinEnds .filter isDigit
    where 
    joinEnds xs = read([head xs])*10 + read([last xs])

answer :: IO Int
answer = inputIO >>= return . foldr1 (+) . map decodeLine

-- Part Two --

one,two,three,four,five,six,seven,eight,nine :: Parser String
one   = string "one"   >> pure "1"
two   = string "two"   >> pure "2"
three = string "three" >> pure "3"
four  = string "four"  >> pure "4"
five  = string "five"  >> pure "5"
six   = string "six"   >> pure "6"
seven = string "seven" >> pure "7"
eight = string "eight" >> pure "8"
nine  = string "nine"  >> pure "9"

rOne,rTwo,rThree,rFour,rFive,rSix,rSeven,rEight,rNine :: Parser String
rOne   = string (reverse "one") >> pure "1"
rTwo   = string (reverse "two") >> pure "2"
rThree = string (reverse "three") >> pure "3"
rFour  = string (reverse "four") >> pure "4"
rFive  = string (reverse "five")  >> pure "5"
rSix   = string (reverse "six")  >> pure "6"
rSeven = string (reverse "seven") >> pure "7"
rEight = string (reverse "eight") >> pure "8"
rNine  = string (reverse "nine") >> pure "9"

fstNum :: Parser String 
fstNum = fmap (head.filter (/= "")) $ many (numberz <|> (letter >> pure ""))
    where 
    numberz = foldr1 (<|>) $ [fmap (:[]) digit, one,two,three,four,five,six,seven,eight,nine] 

lstNum :: Parser String
lstNum = fmap (head.filter (/= "")) $ many (numberz <|> (letter >> pure ""))
    where
    numberz = foldr1 (<|>) $ [fmap (:[]) digit, rOne,rTwo,rThree,rFour,rFive,rSix,rSeven,rEight,rNine] 

calibrationValue :: String -> Int
calibrationValue str = read $ fstNumberz ++ lstNumberz
    where
    fstNumberz = fst.head$parse fstNum str
    lstNumberz = fst.head$parse lstNum$reverse str
     
answer2 :: IO Int
answer2 = do 
           ins <- inputIO 
           return $ sum $ map calibrationValue ins
