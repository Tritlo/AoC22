{-# LANGUAGE GHC2021 #-}

module Util ( module Text.ParserCombinators.ReadP,
             module Debug.Trace,
             parsePrec,
             chunks,
             mdist,
             mergeRange,
             mergeRanges,
             rangeSize,
             lineInput,
             Range,
             bfs,
             bfsEndF,
             dfs
             ) where

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import qualified Text.ParserCombinators.ReadPrec as RP
import Debug.Trace
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Map (Map)
import qualified Data.Map as Map

lineInput :: Read a => FilePath -> IO [a]
lineInput = fmap (map read . lines) . readFile

parsePrec :: ReadP a -> Int -> ReadS a
parsePrec parse = readPrec_to_S (RP.lift parse)

mdist :: (Int, Int) -> (Int,Int) -> Int
mdist (x_1,x_2) (y_1,y_2) = abs (x_1-y_1) + abs (x_2-y_2)


type Range = (Int,Int)

mergeRange :: Range -> Range -> Maybe Range
mergeRange (a,b) (c, d) | c - b <= 1 = Just (a, max b d)
mergeRange _ _ = Nothing

mergeRanges :: [Range] -> [Range]
mergeRanges (a:b:rs) =
     case mergeRange a b of
        Just r -> mergeRanges (r:rs)
        _ -> a:(mergeRanges (b:rs))
mergeRanges rs = rs


rangeSize :: (Int,Int) -> Int
rangeSize (s,e) = (e-s) + 1

chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks n xs | length xs <= n = [xs]
            | otherwise = let (s,r) = splitAt n xs
                          in s : chunks n r


bfs :: Ord a => (a -> [a]) -> a -> a -> Maybe [a]
bfs n s e = bfsEndF n s (\q -> EQ == (compare e q))

bfsEndF :: Ord a => (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
bfsEndF neighbors start endF = bfs' Set.empty Map.empty [start]
  where prioritize (q:qs) = (q,qs) -- standard bfs
        bfs' _ p [] = Nothing
        bfs' _ parents queue | (endF q)  = Just $ reverse (recPath q)
          where (q,qs) = prioritize queue
                recPath c_n | c_n == start = [start]
                recPath c_n =  c_n:(recPath (parents Map.! c_n))
        bfs' seen ps queue = bfs' seen' ps' $ (qs ++ ns)
            where (q,qs) = prioritize queue
                  ns = filter (not . flip Set.member seen) $
                        neighbors q
                  ps' = Map.union ps (Map.fromList $ zip ns (repeat q))
                  seen' = Set.union seen $ Set.fromList ns

dfs :: Ord a => (a -> [a]) -> a -> (a -> Bool) -> Maybe [a]
dfs neighbors start endF = case dfs' Set.empty start of
                              Left _ -> Nothing
                              Right as -> Just as
  where ns = neighbors start
        work seen [] = Left seen
        work seen (a:as) =
             case dfs' (Set.insert a seen) a of
                  Left seen' -> work seen' as
                  Right as' -> Right (a:as')
        dfs' seen cur =
            case filter (not . flip Set.member seen) $ neighbors cur of
                  [] -> Left seen
                  (a:_)  | endF a -> Right [a]
                  as -> work seen as


