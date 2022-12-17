module Util ( module Text.ParserCombinators.ReadP,
             module Debug.Trace,
             parsePrec,
             chunks,
             mdist,
             mergeRange,
             mergeRanges,
             rangeSize,
             lineInput,
             Range
             ) where

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import qualified Text.ParserCombinators.ReadPrec as RP
import Debug.Trace

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