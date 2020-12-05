module Main where

import Advent
import Day1(day1)
import Day2(day2)
import Day3(day3)
import Day4(day4)
import Day5(day5)
import Lib 

main :: IO ()
main = do 
    let d = day5
    (parsed, result1) <- part1debug d
    print result1
    (parsed2, result2) <- part2debug d
    print result2
