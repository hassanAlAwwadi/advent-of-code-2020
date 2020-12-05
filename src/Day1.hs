module Day1(day1) where


import Data.Maybe (mapMaybe)
import Text.Read ( readMaybe )
import Control.Monad(guard)

import Advent ( Advent(..) )

parse :: String -> [Int]
parse = mapMaybe readMaybe . lines

file :: FilePath
file = "Input/Input1.txt"

act1 :: [Int] -> [Int]
act1 ns = do 
    x <- ns
    y <- tail ns
    guard  (x + y == 2020)
    return (x*y)

act2 :: [Int] -> [Int]
act2 ns = do 
    x <- ns 
    y <- tail ns
    z <- tail $ tail ns
    guard (x + y + z == 2020)
    return (x*y*z)

day1 :: Advent [Int] [Int] [Int] [Int]
day1 = Advent {
    files  = (file, file),
    acts   = (act1, act2),
    parses = (parse, parse)
}
