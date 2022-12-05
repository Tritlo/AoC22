{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import Text.ParserCombinators.ReadP
import Data.Char (isDigit)
import Text.Read (readMaybe)
import Data.List (transpose)
import Data.Maybe ( catMaybes )
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)



data Instr = I {num :: Int, source :: Int, dest :: Int} deriving (Show)
data Box = B Char

instance Show Box where
    show (B c) = c:[]

instance Read Box where
  readsPrec i = readP_to_S parse
   where parse = do char '['
                    c <- get
                    char ']'
                    skipSpaces
                    return $ B c

instance Read Instr where
    readsPrec i = readP_to_S parse
      where parse = do string "move"
                       skipSpaces
                       num <- read @Int <$> munch1 isDigit
                       skipSpaces
                       string "from"
                       skipSpaces
                       source <- read @Int <$> munch1 isDigit
                       skipSpaces
                       string "to"
                       skipSpaces
                       dest <- read @Int <$> munch1 isDigit
                       skipSpaces
                       return (I{..})
chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks n xs | length xs <= n = [xs]
            | otherwise = let (s,r) = splitAt n xs
                          in s : chunks n r


readInput :: FilePath -> IO (Stacks,[Instr])
readInput fn = do f <- lines <$> readFile fn
                  let (containers, _:instrs) = break null f
                      base:ccs = reverse $ map (chunks 4) containers
                      numcs = length base
                      parsed_stacks = map (map (readMaybe @Box)) ccs
                      stacks = replicate numcs ([] :: [Box])
                  return (IM.fromList $ zip [1..] $
                             map (reverse . catMaybes) $
                                 transpose parsed_stacks, map (read @Instr) instrs)

type Stacks = IntMap [Box]


task1 :: (Stacks, [Instr]) -> String
task1 (sts, instrs) = IM.foldr firsts "" $ foldl runInstr sts instrs
   where firsts :: [Box] -> String -> String
         firsts [] sf =  sf
         firsts ((B c):_) sf = c:sf
         runInstr  :: Stacks -> Instr -> Stacks
         runInstr stacks (I{..}) = (IM.insert source) source' $
                                 IM.adjust ((reverse mvd) ++) dest stacks
             where (mvd, source') = splitAt num (stacks IM.! source)


task2 :: (Stacks, [Instr]) -> String
task2 (sts, instrs) = IM.foldr firsts "" $ foldl runInstr sts instrs
   where firsts :: [Box] -> String -> String
         firsts [] sf =  sf
         firsts ((B c):_) sf = c:sf
         runInstr  :: Stacks -> Instr -> Stacks
         runInstr stacks (I{..}) = (IM.insert source) source' $
                                    IM.adjust ( mvd ++) dest stacks
            where (mvd, source') = splitAt num (stacks IM.! source)

main :: IO ()
main = do readInput "example" >>= print . task1
          readInput "input" >>= print . task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2