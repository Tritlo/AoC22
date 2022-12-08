{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Data.Char (digitToInt)
import Data.List (transpose, maximumBy)
import Data.Set (Set)
import Data.Function (on)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map

readInput :: FilePath -> IO [[Int]]
readInput = fmap parse . readFile
  where parse :: String -> [[Int]]
        parse = map (map digitToInt) . lines

addCoords :: [[Int]] -> [[((Int,Int),Int)]]
addCoords = zipWith (\ri -> zipWith (\ci v -> ((ri,ci),v)) [0..]) [0..]

variations :: [[((Int,Int),Int)]]  -> [[((Int,Int),Int)]]
variations xs = xs ++ (map reverse xs)
              ++ (transpose xs)
              ++ (map reverse (transpose xs))

seen :: [((Int,Int),Int)] -> [((Int,Int),Int)]
seen = seen' (-1)
  where seen' _ [] = []
        seen' maxSoFar (t@(_,v):r) | v > maxSoFar = t:(seen' v r)
                                   | otherwise = seen' maxSoFar r

data Tree = T {h :: Int, c :: (Int,Int), s :: Map Char Int}

instance Eq Tree where
    (==) = (==) `on` c

instance Ord Tree where
    compare = compare `on` c

instance Show Tree where
    show (T{..}) = (show c) ++ ":" ++ (show (h, Map.elems s))
task1 :: [[Int]] -> Int
task1 = Set.size . Set.unions . map Set.fromList  . map (map fst) . map seen . variations . addCoords

toTree :: ((Int,Int),Int) -> Tree
toTree  (c,v) = T {h = v, c = c, s = Map.fromList [('u',0),
                                                   ('d',0),
                                                   ('l',0),
                                                   ('r',0)]}

score :: Tree -> [Tree] -> Int
score t@T{h=v} rest = sc rest
  where sc ((T{..}):trs) | h < v = 1 + (sc trs)
                         | otherwise = 1
        sc [] = 0

scores :: Char -> [Tree] -> [Tree]
scores _ [] = []
scores k (t:ts) = t':(scores k ts)
   where view_score = score t ts
         t' = t {s = Map.insert k view_score (t.s)}

scoreForest :: [[Tree]] -> [[Tree]]
scoreForest = transpose . map reverse . map (scores 'u')
                  . map reverse . map (scores 'd')
                  . transpose
                  . map reverse . map (scores 'l')
                  . map reverse . map (scores 'r')


totalScore :: Tree -> Int
totalScore = product . Map.elems . s

bestTree :: [[Tree]] -> Tree
bestTree = maximumBy (compare `on` totalScore) .
          map (maximumBy (compare `on` totalScore))

task2 :: [[Int]] -> Int
task2 = totalScore . bestTree . scoreForest . map (map toTree) . addCoords

main :: IO ()
main = do readInput "example" >>= print . task1
          readInput "input" >>= print . task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2