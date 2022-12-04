module Main where

import Data.List (sort)

data Move = Rock | Paper | Scissors deriving (Show, Eq, Ord, Enum)
type Round = (Move, Move)

strToMove :: Char -> Move
strToMove 'A' = Rock
strToMove 'B' = Paper
strToMove 'C' = Scissors
strToMove 'X' = Rock
strToMove 'Y' = Paper
strToMove 'Z' = Scissors



readInput :: FilePath -> IO [Round]
readInput fn = (map parseLine . lines) <$> readFile fn
    where parseLine (c1:' ':c2:[]) = ( strToMove c1, strToMove c2)

selScore :: Move -> Int
selScore Rock = 1
selScore Paper = 2
selScore Scissors = 3

scoreRound :: Move -> Move  -> Int
scoreRound Rock     Paper     = 6
scoreRound Rock     Scissors  = 0
scoreRound Paper    Scissors  = 6
scoreRound Paper    Rock      = 0
scoreRound Scissors Rock      = 6
scoreRound Scissors Paper     = 0
scoreRound _ _  = 3



totalScore :: Round -> Int
totalScore (opp, me) = selScore me + (scoreRound opp me)

part1 :: [Round] -> Int
part1 = sum . map totalScore


part2' :: Round -> Int
part2' (opp, res) = totalScore (opp, otherRes !! otherMoveInd)
  where otherRes :: [Move]
        otherRes = map snd $ sort $ map (\m -> (scoreRound opp m, m)) [Rock, Paper,Scissors]
        otherMoveInd = fromEnum res



part2 :: [Round] -> Int
part2 = sum . map part2'

main :: IO ()
main = do readInput "input" >>= print . part1
          readInput "input" >>= print . part2