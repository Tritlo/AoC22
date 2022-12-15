{-# LANGUAGE  GHC2021 #-}
{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where


import Util
import Data.List (sort)
import Data.Set (Set)
import qualified Data.Set as Set

data Input = S {l :: (Int,Int),
                b :: (Int,Int),
                d :: Int} deriving (Show)

instance Read Input where
    readsPrec = parsePrec parse
       where parse = do string "Sensor at"
                        skipSpaces
                        l_x <- parsePos 'x'
                        char ','
                        skipSpaces
                        l_y <- parsePos 'y'
                        char ':'
                        skipSpaces
                        string "closest beacon is at"
                        skipSpaces
                        b_x <- parsePos 'x'
                        char ','
                        skipSpaces
                        b_y <- parsePos 'y'
                        let l = (l_x,l_y)
                            b = (b_x,b_y)
                            d = mdist l b
                        return (S {..})
             parsePos c = do char c
                             char '='
                             readS_to_P (reads @Int)


readInput :: FilePath -> IO [Input]
readInput = fmap (map read . lines) . readFile

-- task1 :: Int -> [Input] -> [((Int,Int), Int, Int, Int)]
task1 :: Int -> [Input] -> Int
task1 target inps = rsize - (Set.size $ Set.fromList becs)
   where becs =  filter (\(_,b_y) -> b_y == target) $ map (\s -> s.b) inps
         onlyS s = (s.l,s.d)
         proj (c@(x,y), d) = (x, mdist c (x,target), d)
         toRange (x, dist, d) = (x-num_p, x+num_p)
            where num_p = abs (dist - d)
         rsize = sum $ map rangeSize $ mergeRanges $
                      sort $ map toRange $
                      map proj $
                      map onlyS $
                      filter (\(S (_,y) _ d) -> abs (target - y) <= d) inps

task2' :: Int -> [Input] -> [(Int, Int)]
task2' target inps =  ranges
   where becs =  filter (\(_,b_y) -> b_y == target) $ map (\s -> s.b) inps
         onlyS s = (s.l,s.d)
         proj (c@(x,y), d) = (x, mdist c (x,target), d)
         toRange (x, dist, d) = (x-num_p, x+num_p)
            where num_p = abs (dist - d)
         ranges = mergeRanges $
                      sort $ map toRange $
                      map proj $
                      map onlyS $
                      filter (\(S (_,y) _ d) -> abs (target - y) <= d) inps

task2 :: Int -> [Input] -> Int
task2 range inps = (x*4_000_000) + y
  where [(y, [(_,e),_])] = filter ((== 2) . length . snd ) $
                             map (\i ->(i, task2' i inps)) [0..range]
        x = e +1
main :: IO ()
main = do readInput "Day15/example" >>= print . task1 10
          readInput "Day15/input" >>= print . task1 2_000_000
          readInput "Day15/example" >>= print . task2 20
          readInput "Day15/input" >>= print . task2 4_000_000
