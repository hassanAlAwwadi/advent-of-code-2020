{-# LANGUAGE RankNTypes #-}
module Advent where 

data Advent a b c d = Advent {
    files  :: (FilePath, FilePath),
    acts   :: (a -> c, b -> d),
    parses :: (String -> a, String -> b)
}


partDebug :: (forall e. (e,e) -> e) -> Advent a a b b -> IO (a,b)
partDebug zoom Advent {files = fs, acts = as, parses = ps} = do
    let file  = zoom fs
    let act   = zoom as
    let parse = zoom ps
    input <- readFile file
    let parsed = parse input
    let result = act parsed
    return (parsed, result)

part :: (forall e. (e,e) -> e) -> Advent a a b b -> IO b
part zoom a = snd <$> partDebug zoom a


part1 :: Advent a a b b -> IO b
part1 = part fst 

part2 :: Advent a a b b -> IO b
part2 = part snd 

part1debug :: Advent a a b b -> IO (a,b)
part1debug = partDebug fst 

part2debug :: Advent a a b b -> IO (a,b)
part2debug = partDebug snd 
