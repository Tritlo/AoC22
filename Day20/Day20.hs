{-# LANGUAGE GHC2021 #-}
module Main where


import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Tuple (swap)
import Util
import Data.List (elemIndex)

readInput :: FilePath -> IO [Int]
readInput = fmap (map read . lines) . readFile


newPos :: Int -> Int -> Int -> Int
newPos l_li ind v = (ind + v) `mod`  l_li

-- task1 :: [Int] -> [Int]
task1 :: [Int] -> Int
task1 li = sum nums
     --transposes_end
    --  zipWith (newPos l_li) [0..] li
  where l_li = length li
        final_trans = task1' transposes orig_li
        transposes_end = IM.fromList $ map swap $ IM.assocs final_trans
        Just orig_0_i = elemIndex 0 li
        final_0_i = final_trans IM.! orig_0_i
        nums = map (\i ->
                (li !!) $
                transposes_end IM.! ((final_0_i + i) `mod` l_li)) [1_000, 2_000,3_000]

        orig_li = zip [0..] li
        transposes = IM.fromList $ take l_li $ zip [0..] [0..]
        task1' transposes [] = transposes
        task1' transposes ((orig_i, v):rest) = task1' transposes' rest
            where cur_ind = transposes IM.! orig_i
                  new_ind_cand = cur_ind + v
                  new_ind =
                            -- traceShow (applyMix transposes li) $
                            -- traceShow transposes $
                            -- traceShow (v, cur_ind, new_ind_cand) $
                            -- traceShowId $
                             if v < 0
                             then (l_li + new_ind_cand -(1 + new_ind_cand `div` l_li)) `mod` l_li
                             else if new_ind_cand > l_li
                                  then (l_li + new_ind_cand +(1+ (new_ind_cand `div` l_li))) `mod` l_li
                                  else (l_li + new_ind_cand) `mod` l_li
                  transposes' =
                    if cur_ind /= new_ind
                    then IM.map upd transposes
                    else transposes
                  upd ind | ind == cur_ind = new_ind
                  upd ind | cur_ind < new_ind =
                            if ind > cur_ind && ind <= new_ind
                            then ind -1 else ind
                  upd ind | new_ind < cur_ind =
                            if ind >= new_ind && ind < cur_ind
                            then ind +1 else ind


applyMix :: IntMap Int -> [Int] -> [Int]
applyMix transposes orig_li = --transposes_end
    map ((orig_im IM.!) . snd) transposes_end
    where orig_im = IM.fromList $ zip [0..] orig_li
          transposes_end = IM.assocs $ IM.fromList $ map swap $ IM.assocs transposes


main :: IO ()
main = do
            readInput "Day20/example" >>= print . task1
            readInput "Day20/input" >>= print . task1

