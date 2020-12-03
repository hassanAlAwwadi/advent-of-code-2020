module Day3(day3) where 

import Data.Map.Strict as M(Map(..), fromList, lookup) 
import Data.List(unfoldr)
import Control.Monad(join)
import Advent(Advent(..)) 

-- types
data Geology = Geology {
    _geology  :: Map (Int,Int) Char,
    xbound   :: Int,
    ybound   :: Int
} deriving Show

(!?) :: (Int,Int) -> Geology -> Maybe Char
(x,y) !? Geology { _geology = inner, xbound = xl, ybound = yl } 
    = {-trace (show (x,xl, y, yl ))-} M.lookup (x `mod` xl, y) inner
-- file 
file    = "Input/Input3.txt"
example = "Input/Example3.txt"

-- parse 
parse :: String -> Geology
parse input = let 
    innerl = index $ lines input 
    innerm = fromList $ join innerl
    yb = length innerl -1
    xb = fst . fst . last $ head innerl
    in Geology { _geology = innerm, ybound = yb, xbound = xb + 1 }


index :: [[a]] -> [[((Int,Int), a)]]
index = index' 0 where 
    index' :: Int -> [[a]] -> [[((Int,Int), a)]]
    index' _ [] = [] 
    index' y (ns:nss) = index'' (0,y) ns : index' (y+1) nss
    index'' :: (Int,Int) -> [a] -> [((Int,Int), a)]
    index'' _ [] = []
    index'' (x,y) (n:ns) = ((x,y),n) : index'' (x+1,y) ns


-- act 
act1 g = let
    sled (xd,yd) (x,y) = case (x, y) !? g of
        Nothing -> Nothing
        Just c  -> Just (c == '#', (x+xd,y+yd))
    path = unfoldr (sled (3,1)) (3,1)
    hits = filter id path
    in length hits

act2 g = let
    sled (xd,yd) (x,y) = case (x, y) !? g of
        Nothing -> Nothing
        Just c  -> Just (c == '#', (x+xd,y+yd))
    paths = map (\delta -> unfoldr (sled delta) delta) [(1,1), (3,1), (5,1), (7,1), (1,2)]
    hits = filter id <$> paths
    in product (length <$> hits)

--advent
day3 = Advent {
    files  = (file,file),
    parses = (parse,parse),
    acts   = (act1,act2)
}