{-# LANGUAGE GHC2021 #-}
module Main where


import Data.Coerce
import qualified Data.IntMap as IM
import Data.IntMap (IntMap)
import Data.Sequence (Seq(..), (|>), (<|))
import qualified Data.Sequence as Seq
import qualified Data.Foldable as FL
import Data.Maybe (catMaybes)

import Util


newtype Input = I Int

instance Show Input where
    show (I 1) = ">"
    show _ = "<"

instance Read Input where
    readsPrec = parsePrec parse
      where parse :: ReadP Input
            parse = choice [char '>' >> return (I 1),
                            char '<' >> return (I (-1))]

readInput :: FilePath -> IO (Seq Input)
readInput = fmap (Seq.fromList . map (read . (:[]))) . readFile


netEffect :: [Input] -> Int
netEffect = sum . map coerce




type Block = Seq (Maybe Int)
type Floor = Block


blockTops :: [Block->Block]
blockTops =  let btf fs (Nothing:<|r) = Nothing <| (btf fs r)
                 btf (f:fs) (i:<|r) = (f i) <| (btf fs r)
                 btf _ r = r
             in [btf (replicate 4 id),
                 -- ^ ..####.
                 btf [id, fmap (+2), id],
                 -- ^ ...#...
                 --   ..###..
                 --   ...#...
                 btf [id,id, fmap (+2)],
                 -- ^ ....#..
                 --   ....#..
                 --   ..###..
                 btf [fmap (+3)],
                 -- ^ ..#
                 --   ..#
                 --   ..#
                 --   ..#
                 btf (replicate 2 (fmap (+2)))
                 -- ^ ..##...
                 --   ..##...
                 ]
  where n = Nothing
        j = Just

blockBots :: [Block]
blockBots = map Seq.fromList
             [[n,n,j 1, j 1, j 1, j 1, n],
              -- ^ ..####.
              [n, n, j 2, j 1, j 2, n, n],
              -- ^ ...#...
              --   ..###..
              --   ...#...
              [n, n, j 1, j 1, j 1, n, n],
              -- ^ ....#..
              --   ....#..
              --   ..###..
              [n, n, j 1, n, n, n, n],
              -- ^ ..#
              --   ..#
              --   ..#
              --   ..#
              [n, n, j 1, j 1, n, n, n]
              -- ^ ..##...
              --   ..##...
              ]
  where n = Nothing
        j = Just

initialFloor :: Block
initialFloor = Seq.fromList $ replicate 7 (Just 0)

blockStart :: Int -> Int -> Block
blockStart block_t start = fmap (fmap (+start)) bl
    where bl = blockBots !! (block_t `mod` (length blockBots))

blockJet :: Input -> Floor -> Block -> Block
blockJet (I e) fl b | hitsBounds b = b
                    | hitsFloor  = b
                    | otherwise = undefined
 where
    shift e (i :<| r)  | e < 0 = r <| i
    shift e (r :|> i)  | e > 0 = i |> r
    shift _ _ = error "invalid block!!"
    hitsFloor :: Bool
    hitsFloor | e < 0 = hitsFloor' fl b
              | otherwise = hitsFloor' (Seq.reverse fl) (Seq.reverse b)
      where hitsFloor' ((Just i):<|k:<|f1) ((Just j):<|f2) | i >= j = True
                                                           | otherwise = hitsFloor' (k<|f1) f2
            hitsFloor' _ b = False


    hitsBounds bl@((Just _):<|_) | e < 0 = False
    hitsBounds (Nothing:<|r) | e < 0 = True
    hitsBounds bl@(r:|>(Just _)) = False
    hitsBounds (r:|>Nothing) = True

stops :: Block -> Floor -> Floor
stops block floor = Seq.zipWith bump block floor
 where bump (Just i) (Just j) = Just (max i j)
       bump Nothing j = j
       bump j Nothing = j

collides :: Floor -> Block -> Bool
collides (Nothing:<|b) (_:<|f) = collides b f
collides ((Just _):<|b) (Nothing:<|f) = collides b f
collides ((Just i):<|b) ((Just j):<|f) | j >= i = True
                                       | otherwise = collides b f
collides _ _ = False

simulate :: Int -> Floor -> Block -> Seq Input -> Floor
simulate num_block floor _ _ | (traceShow (num_block, floor)
                               num_block) == 4 = floor
simulate num_blocks fl bl (inp:<|inps)
     | collides (falls bl_jet) fl
         = let floor' = stops (traceShow (bl_jet, bl_top_f bl_jet) (bl_top_f bl_jet)) fl
               bl_top_f = blockTops !! (num_blocks `mod` (length blockTops))
               nb' = num_blocks + 1
               max_block = maximum $ catMaybes $ FL.toList floor'
           in simulate (num_blocks +1) floor' (blockStart nb' (max_block + 3)) inps'

     | otherwise = simulate num_blocks fl (falls bl_jet) (inps |>inp)
  where bl_jet =  traceShowId $ blockJet inp fl bl
        inps' = inps |> inp
falls :: Block -> Block
falls = fmap (fmap (\x -> x-1))

main :: IO ()
main = do inps <- readInput "Day17/example"
          print initialFloor
          print $ simulate 0 initialFloor (blockStart 0 3) inps