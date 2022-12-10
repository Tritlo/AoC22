{-# LANGUAGE  GHC2021 #-}
{-# LANGUAGE OverloadedRecordDot, RecordWildCards #-}
module Main where

import qualified Text.ParserCombinators.ReadPrec as RP ( readPrec_to_S, lift )
import Text.ParserCombinators.ReadP ( choice, readS_to_P, skipSpaces, string )
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy

data Instr = AddX Int
           | Noop deriving (Show)


instance Read Instr where
   readsPrec = RP.readPrec_to_S (RP.lift parse)
     where parse = choice [parseAddX, parseNoop]
           parseNoop = string "noop" >> return Noop
           parseAddX = do string "addx"
                          skipSpaces
                          AddX <$> (readS_to_P $ reads @Int)


cycles :: Instr -> Int
cycles (AddX {}) = 2
cycles Noop = 1

readInput :: FilePath -> IO [Instr]
readInput = fmap (map read . lines) . readFile


data VMState = VMS {cur_cycle :: Int, registers :: Map Char Int}

initialState :: VMState
initialState = VMS {cur_cycle = 1, registers = Map.fromList [('X', 1)]}

type VM = State VMState

exec :: Instr -> VM ()
exec Noop = modify (\VMS{..} -> VMS {cur_cycle = cur_cycle + 1,..})
exec (AddX v) = modify (\VMS{..} -> VMS {cur_cycle=cur_cycle+2,
                                         registers= Map.adjust (+v) 'X' registers})

observe :: [Instr] -> VM [(Int,Int)]
observe [] = return []
observe (instr:instrs) = do exec instr
                            VMS{..} <- get
                            ((cur_cycle, registers Map.! 'X'):) <$> observe instrs

task1 :: [Instr] -> Int
task1 instrs =
       (sum . zipWith (*) (20:[60, 100..]) . map snd) $
        init $ signals 20 $ evalState (observe instrs) initialState

signals :: Int -> [(Int,Int)] -> [(Int,Int)]
signals _ [] = []
signals n vals = s:(signals (n+40) af)
   where (bf,af) = span (\(c,_) -> c <= n) vals
         (s:_) = reverse bf

between :: Int -> [(Int,Int)] -> [[(Int,Int)]]
between n vals = between' 0 vals
 where between' _ [] = []
       between' cur vals = bf:(between' (cur+n) af)
          where (bf,af) = span (\(c,_) -> c <= cur+n) vals


task2 :: [Instr] -> [(Int,Int)]
task2 instrs = evalState (observe instrs) initialState


draw :: Int -> Int -> [(Int,Int)] -> String
draw _ cv [] = []
draw cur_p cv vs@((cycle,upd):rest) =
        output:(if screen_pos == 39 then '\n':next else next)

  where screen_pos = cur_p `mod` 40
        next = (draw (cur_p +1) cv' vs')
        (cv',vs')  = if cur_p == (cycle -1)
                     then (upd,rest) else (cv,vs)
        output = if abs (screen_pos - cv') < 2 then '█' else ' '



main :: IO ()
main = do readInput "input" >>= print . task1
          readInput "example-large" >>= print .  task1
          readInput "example-large" >>= putStrLn . draw 1 1 . task2
          readInput "input" >>= putStrLn . draw 0 1 . task2