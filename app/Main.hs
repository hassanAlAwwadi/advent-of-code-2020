module Main where

import Advent
import Day1(day1)
import Day2(day2)
import Lib 

main :: IO ()
main = do 
    let d = day1
    (parsed, result1) <- part1debug day2
    print result1
    (parsed2, result2) <- part2debug day2
    print result2
