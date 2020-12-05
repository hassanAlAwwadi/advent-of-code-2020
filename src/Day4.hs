{-# LANGUAGE LambdaCase #-}
module Day4(day4) where

import Data.List.Split(splitOn)
import Data.Char(isDigit)
import Data.List(all)
import Text.Read ( readMaybe )
import Data.Map.Strict as M(Map(..), fromList, lookup) 
import Control.Monad(join)
import Advent (Advent(..))
-- types 

type Passport = Map String String
type Required = [String]
type RequiredStrict = [(String, String -> Bool)]


-- util

allMaybe :: (a -> Maybe b) -> [a] -> Bool
allMaybe _ [] = True
allMaybe q (x:xs) = case q x of 
    Nothing -> False
    _       -> allMaybe q xs 

allMaybeBool ::  (a -> Maybe Bool) -> [a] -> Bool
allMaybeBool _ [] = True
allMaybeBool q (x:xs) = case q x of 
    Nothing   -> False
    Just True -> allMaybeBool q xs 
    _         -> False
-- files 
file = "Input/Input4.txt"
example = "Input/Example4.txt"

-- parse 
parse :: String -> [Passport]
parse s = let 
    raw = words <$> fixemptylineissue (lines s)
    parse' :: [String] -> Passport
    parse' ss = fromList $ (\s -> let (k:ak) = splitOn ":"  s in (k, join ak)) <$> ss
    in parse' <$> raw

fixemptylineissue :: [String] -> [String]
fixemptylineissue [] = [] 
fixemptylineissue [x] = [x] 
fixemptylineissue (x:"":xs) = x : fixemptylineissue xs 
fixemptylineissue (x:y:zs)  = fixemptylineissue $ join [x, " ", y]:zs

-- act

-- 1
act1 :: Required -> [Passport] -> Int
act1 r = act1 where 
    act1 :: [Passport] -> Int
    act1 p  = length $ filter valid p
    valid :: Passport -> Bool
    valid passport = allMaybe (`M.lookup` passport) r
required1 :: Required
required1 = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"]

-- 2
act2 :: RequiredStrict -> [Passport] -> Int
act2 r = act2 where 
    act2 :: [Passport] -> Int
    act2 p  = length $ filter valid p
    valid :: Passport -> Bool
    valid passport = allMaybeBool (\(k, q) -> q <$> k `M.lookup` passport) r

required2 :: RequiredStrict
required2 = 
    [ ("byr", \s -> length s == 4 && (case readMaybe s of Just n -> n >= 1920 && n <= 2002 ; _ -> False))
    , ("iyr", \s -> length s == 4 && (case readMaybe s of Just n -> n >= 2010 && n <= 2020 ; _ -> False))
    , ("eyr", \s -> length s == 4 && (case readMaybe s of Just n -> n >= 2020 && n <= 2030 ; _ -> False))
    , ("hgt", \case
        (n:m:x:"cm")-> (case readMaybe [n,m,x] of 
            Just y  -> y >= 150 && y <= 193 
            _       -> False)
        (n:m:"in")  -> (case readMaybe [n,m] of 
            Just y  -> y >= 59 && y <= 76 
            _       -> False)
        _           -> False)
    , ("hcl", \case
        '#':rest -> length rest == 6 && all (`elem` "0123456789abcdef") rest
        _        -> False)
    , ("ecl", \s -> s `elem` ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"])
    , ("pid", \s -> length s == 9 && all isDigit s)
    ]

--advent
day4 = Advent {
    files  = (file,file),
    parses = (parse,parse),
    acts   = (act1 required1, act2 required2)
}