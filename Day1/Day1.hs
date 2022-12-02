{-# LANGUAGE TypeApplications #-}
module Main where

import Data.List (sort)

readInput :: FilePath -> IO [[Int]]
readInput filename =
     do file <- lines <$> readFile filename
        --print file
        let elfs [] = []
            elfs xs = x:(case y of
                            [] -> []
                            _:y -> elfs y)
              where (x,y) = span (/= "") xs
        return $ map (map (read @Int)) $ elfs file


prob1 :: [[Int]] -> Int
prob1 = maximum . map sum

prob2 :: [[Int]] -> Int
prob2 = sum . take 3 . reverse  . sort . map sum

main :: IO ()
main = do readInput "example" >>= (print . prob1)
          readInput "input" >>= (print . prob1)
          readInput "example" >>= (print . prob2)
          readInput "input" >>= (print . prob2)


