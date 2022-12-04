{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import Data.Char (digitToInt, isDigit)
import Data.List (sort)

import Text.ParserCombinators.ReadP as RP


type Range = (Int, Int)
type ElfPair = (Range, Range)

instance {-# OVERLAPPING #-} Read Range where
    readsPrec _ = readP_to_S rinst
      where rinst = do x <- munch1 isDigit
                       char '-'
                       y <- munch1 isDigit
                       return (read @Int x, read @Int y)

instance {-# OVERLAPPING #-} Read ElfPair where
    readsPrec i = readP_to_S epinst
      where epinst = do [e1,e2] <- sepBy (readS_to_P (readsPrec @Range i)) (char ',')
                        return (e1,e2)

readInput :: FilePath -> IO [ElfPair] --[(Range, Range)]
readInput = fmap (map (read @ElfPair) . lines) . readFile

splitOn :: Eq a => a -> [a] -> [[a]]
splitOn x xs = case r of
                 [] -> [b]
                 (_:r') -> b:(splitOn x r')
  where (b,r) = span (x/=) xs


fullyContains :: Range -> Range -> Bool
fullyContains (x1,x2) (y1,y2) = x1 <= y1 && y2 <= x2
                             || y1 <= x1 && x2 <= y2

overlap :: Range -> Range -> Bool
overlap r1 r2 = overlap' r1 r2 || overlap' r2 r1
  where overlap' (x1,x2) (y1,y2) = x1 <= y1 && y1 <= x2



task1 :: [(Range, Range)] -> Int
task1 = length . filter (uncurry fullyContains)

task2 :: [(Range, Range)] -> Int
task2 = length . filter (uncurry overlap)

main :: IO ()
main = do readInput "example" >>= print . task1
          readInput "input" >>= print . task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2