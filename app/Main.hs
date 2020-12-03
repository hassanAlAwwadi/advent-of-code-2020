module Main where

import Advent
import Day1(day1)
import Day2(day2)
import Day3(day3)
import Lib 

main :: IO ()
main = do 
    let d = day3
    (parsed, result1) <- part1debug d
    print result1
    (parsed2, result2) <- part2debug d
    print result2
