module Day5 where
import Debug.Trace(trace)
import Advent(Advent(..))
-- test input
test1 = "BFFFBBFRRR"
test2 = "FFFBBBFRRR"
test3 = "BBFFBBFRLL"

-- file 

file = "Input/Input5.txt"

-- util
hrange = (0,127)
wrange = (0,7)

zoom :: (Int,Int) -> [Int] -> Int
zoom (n,m) []
    | n == m = n 
    | otherwise = error "not enough commands to zoom in fully"
zoom (n,m) (0:xs) = zoom (lowerhalf (n,m)) xs
zoom (n,m) (1:xs) = zoom (upperhalf (n,m)) xs

lowerhalf (n,m) = let 
    divide = (n + m) `div` 2
    in (n, divide)

upperhalf (n,m) = let 
    (divide,r) = (n + m) `divMod` 2
    in (divide + r, m)

-- parse
parse :: String -> [([Int],[Int])]
parse s = parseline <$> lines s
parseline str = let 
    (l,r) = splitAt 7 str
    in (toBinH l, toBinB r)

toBinH = map (\x -> if x == 'B' then 1 else 0)
toBinB = map (\x -> if x == 'R' then 1 else 0)

-- act
act1 :: [([Int], [Int])] -> Int
act1 l = maximum $ map (act' hrange wrange) l

act' hr wr (hz,wz) = let 
    y = zoom hr hz
    x = zoom wr wz
    in (y*8) + x

act2 :: [([Int], [Int])] -> Int
act2 l = findMySeat $ map (act' hrange wrange) l

findMySeat :: [Int] -> Int
findMySeat taken = let
    allSeats = [0..(127*8)+7]
    untakenSeats  = filter (`notElem` taken)  allSeats
    mine = nonSeq untakenSeats
    in mine


nonSeq :: (Num a, Eq a) => [a] -> a
nonSeq (x:y:z:r) 
    | x /= y-1 && z /= y+1 = y
    | otherwise  = nonSeq (y:z:r)
nonSeq [] = -1
nonSeq _  = -1

-- advent
day5 =  Advent{
    files  = (file,file),
    parses = (parse,parse),
    acts   = (act1, act2)
}