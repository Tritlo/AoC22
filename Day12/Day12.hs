{-# LANGUAGE GHC2021 #-}
module Main where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap as IM
import Data.Maybe (mapMaybe, catMaybes)
import Data.List (findIndex, sortOn)
import Debug.Trace
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

readInput :: String ->IO [[Int]]
readInput = fmap (map (map f) . lines)
          . readFile
  where f 'S' = s_val
        f 'E' = e_val
        f c = fromEnum c

s_val, e_val :: Int
s_val = (fromEnum 'a') - 1
e_val = (fromEnum 'z') + 1

toArray :: [[Int]] -> ((IntMap (IntMap Int), (Int,Int)), (Int,Int))
toArray xs = ((graph, (sx,sy)), (ex,ey))
    where sxs = map (findIndex (== s_val)) xs
          (sx:_) = catMaybes sxs
          Just sy = findIndex (== (Just sx)) sxs
          exs = map (findIndex (== e_val)) xs
          (ex:_) = catMaybes exs
          Just ey = findIndex (== (Just ex)) exs
          graph = IM.fromList $
                     zipWith (\rn row ->
                            (rn, IM.fromList $ zip [0..] row))
                            [0..] xs




neighbors :: IntMap (IntMap Int) -> (Int, Int) -> [(Int,Int)]
neighbors graph lu@(x,y) = map fst $
                        filter (\(c,v) -> v <= xyv + 1 && c /=  lu) $
                        mapMaybe lookup coords
  where coords :: [(Int,Int)]
        xyv = (graph IM.! y) IM.! x
        coords = map (\(i,j) -> (x+i,y+j)) $ [(-1,0), (1,0), (0,1), (0,-1)]
        lookup :: (Int,Int) -> Maybe ((Int,Int),Int)
        lookup (x,y) = fmap (\v -> ((x,y),v)) $ (graph IM.!? y) >>= (IM.!? x)

bfs :: IntMap (IntMap Int) -> (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
bfs graph start end = bfs' Set.empty Map.empty [start]
  where prioritize (q:qs) = (q,qs) -- standard bfs
        bfs' :: Set (Int,Int) -> Map (Int,Int) (Int,Int)
             -> [(Int,Int)] -> Maybe [(Int,Int)]
        bfs' _ p [] = Nothing
        bfs' _ parents queue | q == end = Just $ reverse (recPath end)
          where (q,qs) = prioritize queue
                recPath c_n | c_n == start = [start]
                recPath c_n =  c_n:(recPath (parents Map.! c_n))
        bfs' seen ps queue = bfs' seen' ps' $ (qs ++ ns)
            where (q,qs) = prioritize queue
                  ns = filter (not . flip Set.member seen) $
                        neighbors graph q
                  ps' = Map.union ps (Map.fromList $ zip ns (repeat q))
                  seen' = Set.union seen $ Set.fromList ns


task1 :: [[Int]] -> Int
task1 = fromJust . fmap ((\x -> x-1) . length)
                .  (uncurry $ uncurry bfs) . toArray

task2 :: [[Int]] -> Int
task2 inp = minimum $ mapMaybe (\a -> (\x -> x-1) . length <$> bfs g a e) as
   where as = map fst $ filter (\(_,c) -> c == fromEnum 'a') $ concat lns
         lns = (zipWith (\y ln -> zipWith (\x c -> ((x,y),c)) [0..] ln)) [0..] inp
         ((g,_),e) = toArray inp

main :: IO ()
main = do readInput "example" >>= print . task1
          readInput "input" >>= print . task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2