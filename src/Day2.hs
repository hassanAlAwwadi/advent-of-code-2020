module Day2(day2) where


import Data.Maybe (mapMaybe)
import Text.Read ( readMaybe )
import Control.Monad(guard, join)
import Data.List.Split(splitOn)
import Debug.Trace
import Advent ( Advent(..) )



-- types
data Rule = Rule {
    rlow :: Int,
    rhig :: Int,
    rkey  :: Char
} deriving (Show, Eq, Ord, Read)

type Password = String

-- file 
file = "Input/Input2.txt"

-- parse 
parse :: String -> [(Rule, Password)]
parse s = let 
    ss = lines s
    in mapMaybe parseLine ss

parseLine :: String -> Maybe (Rule, Password)
parseLine line = let
    minc:afterMin = splitOn "-" line
    maxc:afterMax = splitOn " " (join afterMin)
    keyc:afterKey = splitOn ":" (join afterMax)
    in do 
        min <- readMaybe minc
        max <- readMaybe maxc
        key <- headMaybe keyc
        return (Rule {rlow =  min, rhig = max, rkey = key}, join afterKey)

headMaybe :: [a] -> Maybe a
headMaybe []        = Nothing
headMaybe (c:_)    = Just c


-- act 
-- 1
act1 ::  [(Rule, Password)] -> Int
act1 = length . filter valid1

valid1 :: (Rule, Password) -> Bool
valid1 ( Rule{ rlow = min, rhig = max, rkey = key } , password ) =  let
    count = length $ filter (==key) password
    in count >= min && count <= max


-- 2
act2 ::  [(Rule, Password)] -> Int
act2 = length . filter valid2

valid2 :: (Rule, Password) -> Bool
valid2 ( Rule{ rlow = low, rhig = hig, rkey = key } , password ) =  let
    lowc = key `isAt` (low-1) $ password
    higc = key `isAt` (hig-1) $ password
    in (lowc || higc) && (lowc /= higc)


isAt :: Eq a => a -> Int -> [a] -> Bool
isAt _ _ [] = False
isAt k 0 (c:_) = k == c
isAt k n (_:cs) = isAt k (n-1) cs 
-- advent 
day2 = Advent {
    files  = (file, file),
    acts   = (act1, act2),
    parses = (parse, parse)
}

