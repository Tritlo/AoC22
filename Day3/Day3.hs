module Main where
import Data.Char (isLower)
import Data.List (intersect)
import qualified Data.Set as Set
import Data.Set (Set)

readInput :: FilePath -> IO [[Int]]
readInput fn = (map (map charToPrio) . lines) <$>  readFile fn
 where charToPrio :: Char -> Int
       charToPrio c | isLower c = iv - 96
                    | otherwise = iv - 38
         where iv = fromEnum c


task1 :: [Int] -> Int
task1 xs = minimum (Set.intersection (Set.fromList comp1) (Set.fromList comp2))
  where l = length xs
        (comp1, comp2) = splitAt (l `div` 2) xs


chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks n xs | length xs <= n = [xs]
            | otherwise = let (s,r) = splitAt n xs
                          in s : chunks n r

task2 :: [[Int]] -> Int
task2 inp = sum $ map minimum inters
  where threes :: [[[Int]]]
        threes =  chunks 3 inp
        sets :: [[Set Int]]
        sets = map (map Set.fromList) threes
        inters :: [Set Int]
        inters = map (foldl1 Set.intersection) sets

main :: IO ()
main = do readInput "example" >>= print . sum . map task1
          readInput "input" >>= print . sum . map task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2
