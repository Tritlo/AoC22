module Main where

import qualified Data.Set as Set

readInput :: FilePath -> IO [String]
readInput = fmap lines . readFile

task1 :: String -> Int
task1 inp = task1' 4 inp
  where task1' n (c1:c2:c3:c4:r) | Set.size s == 4 = n
                                 | otherwise = task1' (n+1) (c2:c3:c4:r)
            where s = Set.fromList [c1,c2,c3,c4]
        task1' _ _ = error "end of input without marker!!"

task2 :: String -> Int
task2 inp = task1' 14 (Set.fromList s) s r
   where (s,r) = splitAt 14 inp
         task1' n cur_set sq@(pc:pcs) (r:rs)
                | Set.size cur_set == 14 = n
                | otherwise = task1' (n+1) cur_set'  (pcs ++ [r]) rs
                   where pcs' = pcs ++ [r]
                         cur_set' = Set.fromList pcs'


main :: IO ()
main = do readInput "example" >>= print . map task1
          readInput "input" >>= print . map task1
          readInput "example" >>= print . map task2
          readInput "input" >>= print . map task2