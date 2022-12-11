{-# LANGUAGE GHC2021 #-}
{-# LANGUAGE RecordWildCards #-}
module Main where


import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.ParserCombinators.ReadP
import Text.Read (reads)
import Data.List (partition, sort, foldl')

import Debug.Trace
import qualified Data.IntMap.Strict as IM
import Data.IntMap.Strict (IntMap)


data Monkey = M {starting_items :: [Integer],
                 operation :: Integer -> Integer,
                 test  :: Integer,
                 true_dest :: Int,
                 false_dest :: Int,
                 number :: Int }

instance Show Monkey where
    show (M{..}) = "M " ++ (show number)



instance Read Monkey where
    readsPrec = readPrec_to_S (RP.lift parse)
        where parse = do string "Monkey"
                         skipSpaces
                         number <- readS_to_P (reads @Int)
                         char ':'
                         skipSpaces
                         string "Starting items:"
                         skipSpaces
                         starting_items <- sepBy1 (readS_to_P (reads @Integer)) (char ',')
                         skipSpaces
                         operation <- parseOp
                         skipSpaces
                         test <- parseTest
                         skipSpaces
                         rest <- look
                         true_dest <- parseCond "true"
                         skipSpaces
                         false_dest <- parseCond "false"
                         skipSpaces
                         return $ M {..}
              parseCond str = do string "If"
                                 skipSpaces
                                 string str
                                 string ": throw to monkey"
                                 skipSpaces
                                 readS_to_P (reads @Int)

              parseTest = do string "Test: divisible by"
                             skipSpaces
                             readS_to_P (reads @Integer)
              parseOp = do string "Operation: new ="
                           skipSpaces
                           string "old"
                           skipSpaces
                           op <- choice [char '*' >> return (*),
                                         char '+' >> return (+)]
                           skipSpaces
                           choice [string "old" >> return (\old -> op old old),
                                      do i <- readS_to_P $ reads @Integer
                                         return (\old -> op old i)]




chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks n xs | length xs <= n = [xs]
            | otherwise = let (s,r) = splitAt n xs
                          in s : chunks n r

monkeyTurn :: IntMap (Int, [Integer]) -> Monkey -> IntMap (Int,[Integer])
monkeyTurn in_air (M{..}) = IM.adjust (flip add test_t ) true_dest  $
                                IM.adjust (flip add test_f) false_dest $
                                IM.adjust (\(c,_) -> (c+ (length cur_items), [])) number in_air
    where (test_t, test_f) = partition ((0 == ) . (`mod` test)) $
                                 map (flip div 3 . operation) $ cur_items
          add (c,ls) ls' = (c,ls++ls')
          cur_items = snd $ in_air IM.! number

-- same but no div?
monkeyTurn2 :: IntMap (Int, [Integer]) -> Monkey -> IntMap (Int,[Integer])
monkeyTurn2 in_air (M{..}) = IM.adjust (flip add test_t ) true_dest  $
                                IM.adjust (flip add test_f) false_dest $
                                IM.adjust (\(c,_) -> (c +length cur_items, []))
                                     number in_air
    where (test_t, test_f) = partition ((0 == ) . (`mod` test)) $ map operation $ cur_items
          add (c,ls) ls' = (c,ls++ls')
          cur_items = snd $ in_air IM.! number

task1 :: [Monkey] -> Int
task1 monkeys =  product $ take 2 $ reverse $ sort $ IM.elems $
                 fmap fst $ applyN 20 (turn monkeys) initial_map
  where initial_map = IM.fromList $
                        map (\M{..} -> (number, (0, starting_items))) monkeys
        turn :: [Monkey] -> IntMap (Int, [Integer]) -> IntMap (Int, [Integer])
        turn monkeys = flip (foldl monkeyTurn) monkeys

task2 :: [Monkey] -> Integer
task2 monkeys =  product $
                 map fromIntegral $
                 take 2 $ reverse $ sort $ IM.elems $
                 fmap fst $
                 applyN 10000 (fmap doMod . turn monkeys) initial_map
  where initial_map = IM.fromList $
                        map (\M{..} -> (number, (0, starting_items))) monkeys
        all_tests = product $ map test monkeys
        doMod :: (Int, [Integer]) -> (Int, [Integer])
        doMod (i,ns) = (i,map (`mod` all_tests) ns)
        -- THE pattern REPEATS!!
        -- [ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9,10,11,12,13,14]
        --  xy        x     y  x        x  y     x
        -- [15,16,17,18,19,20,21,22,23,24,25,26,27,28,29]
        --  xy        x     y  x        x y      x
        -- so we can do it via modulo and remain within a ring
        turn :: [Monkey] -> IntMap (Int, [Integer]) -> IntMap (Int, [Integer])
        turn monkeys = flip (foldl' monkeyTurn2) monkeys

applyN :: Int -> (a -> a) -> a -> a
applyN 0 _ i = i
applyN n f a = applyN (n-1) f $! (f a)

readInput :: FilePath -> IO [Monkey]
readInput = fmap (map (read @Monkey . unlines) . chunks 7 . lines ) .  readFile
main :: IO ()
main = do readInput "example" >>= print . task1
          readInput "input" >>= print . task1
          readInput "example" >>= print . task2
          readInput "input" >>= print . task2
