{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Coerce
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.Sequence (Seq(..), (|>), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as FL
import Data.Traversable (traverse)
import Data.Maybe (catMaybes)
import Control.Monad ( forM_ )
import Data.Maybe (listToMaybe)
import qualified Data.Set as Set
import Data.Set (Set)
import Data.Either
import Data.Map (Map)
import qualified Data.Map as Map

import Util


newtype Input = I Bool deriving (Eq, Ord)

instance Show Input where
    show (I True) = ">"
    show _ = "<"

instance Read Input where
    readsPrec = parsePrec parse
      where parse :: ReadP Input
            parse = choice [char '>' >> return (I True),
                            char '<' >> return (I False)]

readInput :: FilePath -> IO (Seq Input)
readInput = fmap (Seq.fromList . map (read . (:[]))) . readFile

type Block = IntMap IntSet
type Floor = IntMap IntSet

shiftRight, shiftLeft :: Block -> Block
shiftRight = IM.map sr
  where sr = IS.mapMonotonic (+1)
shiftLeft = IM.map sl
  where sl = IS.mapMonotonic (\x -> x-1)

collision :: Floor -> Block -> Bool
collision fl bl = or is
  where is :: IntMap Bool
        is = IM.intersectionWith (\f b -> not $ IS.disjoint f b) fl bl

yMod :: Int -> Block -> Block
yMod i = IM.fromDistinctAscList . map (\(x,y) -> (x+i,y)) . IM.assocs

fall :: Block -> Block
fall = yMod (-1)


renderFloor :: Floor -> IO ()
renderFloor fl = do forM_ (reverse [1..max_y+4]) $ \y -> do
                     forM_ [0..6] $ \x ->
                         putStr (if check (x,y) then "#" else ".")
                     putStrLn ""
                    putStrLn (replicate 7 '-')
  where (max_y, _) = IM.findMax fl
        check (x,y) | Just xs <- (fl IM.!? y) = x `IS.member` xs
                    | otherwise = False


blocks :: [Block]
blocks = map IM.fromList $ [[(0, IS.fromList ([0..3]))],
                             -- ^ ####
                            [(0, IS.singleton 1),
                             (1, IS.fromList [0..2]),
                             (2, IS.singleton 1)],
                            -- ^ .#.
                            --   ###
                            --   .#.
                            [(2, IS.singleton 2),
                             (1, IS.singleton 2),
                             (0, IS.fromList [0..2])],
                            -- ^ ..#
                            --   ..#
                            --   ###
                            zip [0..3] $ repeat (IS.singleton 0),
                            -- ^ #
                            --   #
                            --   #
                            --   #
                            zip [0..1] $ repeat (IS.fromList [0..1])
                            -- ^ ##
                            --   ##
                            ]

initialFloor :: Floor
initialFloor = IM.singleton 0 (IS.fromDistinctAscList [0..6])

jet :: Input -> Block -> Block
jet (I True) = shiftRight
jet (I False) = shiftLeft

newBlock :: Integer -> Int -> Block
newBlock num_blocks max_y =
        yMod (max_y+4) $
        shiftRight $ shiftRight $
        blocks !! (fromInteger (num_blocks `mod` (fromIntegral $ length blocks)))



simulate :: Bool -> Integer -> Seq Input -> (Integer, Floor)
simulate short max_block initial_inp =
     simulate' Map.empty 0 0 initialFloor (newBlock 0 0) initial_inp

 where
     simulate' :: Map (Block, Floor, Seq Input)
                              (Integer, (Integer, Floor)) -> Integer ->
                  Integer -> Floor -> Block -> Seq Input -> (Integer, Floor)
     simulate' _ num_block offset floor _ _ | num_block == max_block =
        (offset, floor)
     simulate' seen num_block offset floor bl orig@(inp:<|inps)
         | short,
           Just (first_seen,(orig_os, orig_fl)) <- seen Map.!? lk_up =
           let loop_size = num_block  - first_seen
               divs = (max_block - first_seen) `div` loop_size
               outstanding = max_block - (first_seen + divs * loop_size)
               os_per_loop = offset - orig_os
               (os_outst,fl_final) = simulate' Map.empty (max_block - outstanding) 0 floor bl orig
           in (orig_os + (fromIntegral divs)*os_per_loop + os_outst, fl_final)
         | collision floor down_1 =
             let ns =  if compressed
                       then Map.insert lk_up (num_block, (offset, floor)) seen else seen
             in simulate' ns (num_block +1) offset' floor' new_block inps'
         | otherwise = simulate' seen num_block offset floor down_1 inps'

        where lk_up = (bl, floor, orig)
              bl_shifted = jet inp bl
              bl' = if   outOfBounds bl_shifted
                      || collision floor bl_shifted
                  then bl else bl_shifted

              fl' = IM.unionWith (IS.union) floor bl'
              nfs = newFloor fl'
              (offset', floor', compressed) =  case nfs of
                          Just i ->
                                let f' = IM.fromDistinctAscList $
                                             map (\(k,v) -> (k-i, v)) $
                                             drop i $ IM.toAscList fl'
                                in (offset + (fromIntegral i), f', True)
                          _ -> (offset, fl', False)
              down_1 = fall bl'
              new_block = newBlock (num_block +1) $ fst $ IM.findMax floor'
              inps' = inps |> inp

newFloor :: Floor -> Maybe Int
newFloor = listToMaybe . drop 1 . map fst . filter ((== floor) . snd ) .IM.assocs
  where floor = IS.fromList [0..6]

outOfBounds :: Block -> Bool
outOfBounds bl =  min_is < 0 || max_is >= 7
 where is = IS.unions bl
       min_is = IS.findMin is
       max_is = IS.findMax is

task1 :: Seq Input -> Integer
task1 = fst . simulate True 2022

task2 :: Seq Input -> Integer
task2 = fst . simulate True 1_000_000_000_000

num_final :: Integer
num_final = 1_000_000_000_000

main :: IO ()
main = do
          (i,fl) <- simulate True num_final <$> readInput "Day17/input"
          print (i, IM.findMax fl)
          print (i + (fromIntegral $ fst $ IM.findMax fl))