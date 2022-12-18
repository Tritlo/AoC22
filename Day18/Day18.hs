{-# LANGUAGE  GHC2021 #-}
module Main where

import Util
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (groupBy, partition, minimumBy, maximumBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Maybe (isJust)


data Input = I (Int,Int,Int)

instance Read Input where
    readsPrec  = parsePrec parse
      where parse = do [x,y,z] <- sepBy (readS_to_P reads) (char ',')
                       return $ I (x,y,z)

iToC (I x) = x

readInput :: FilePath -> IO [(Int,Int,Int)]
readInput = fmap (map (iToC . read) . lines) . readFile

type Cube = IntMap (IntMap IntSet)

toSet :: (Int,Int,Int) -> Cube
toSet (x,y,z) = IM.singleton x (IM.singleton y $ IS.singleton z)

cubeUnion :: Cube -> Cube -> Cube
cubeUnion = IM.unionWith (IM.unionWith IS.union)

emptyCube :: Cube
emptyCube = IM.empty

toCube :: [(Int,Int,Int)] -> Cube
toCube = foldl cubeUnion emptyCube . map toSet


-- rotate :: Cube -> [(Int, [(Int, Int)])]
rotate =    toCube .
            map (\(x,y,z) -> (z, x,y)).
            constituentCubes


tinyExample = toCube [(1,1,1), (2,1,1)]

constituentCubes :: Cube -> [(Int,Int,Int)]
constituentCubes =
            map (\(x, (y,z)) -> (x,y,z)) .
            concatMap (\(x, ynz) -> zip (repeat x) ynz) .
            map (\(x, (y, zs)) -> (x, zip (repeat y) zs)) .
            concatMap (\(x,ys) -> zip (repeat x) ys) .
            map (\(x,ys) -> (x,map (\(y,zs) -> (y, IS.toList zs)) $ IM.assocs ys)) . IM.assocs

joined :: Cube -> Int
joined = IM.foldl (\c v -> c + (sjoin v)) 0
  where sjoin :: IntMap IntSet -> Int
        sjoin = IM.foldl (\c is -> c + (length $ contiguous $ IS.toList is ))0

contiguous :: [Int] -> [[Int]]
contiguous [] = []
contiguous (x:y:r) | (x + 1 == y) = let (rc:rest) = contiguous (y:r)
                                     in (x:rc):rest
                   | otherwise = [x]:(contiguous (y:r))
contiguous [x] = [[x]]

sides :: Cube -> Int
sides c = 2*(sum $ map joined [c,rc,rrc])
    where rc = rotate c
          rrc = rotate rc

task1 :: [(Int,Int,Int)] -> Int
task1 = sides . toCube


gaps :: Cube -> [(Int,Int,Int)]
gaps =  IM.foldlWithKey (\c x dim -> c ++ (gaps' x dim)) []
 where gaps' x = IM.foldlWithKey
                     (\c y is -> c ++ gaps'' x y is) []
       gaps'' x y is = map ((x,y,)) $
                         filter (not . flip IS.member is) $
                          [min_is..max_is]
        where min_is = IS.findMin is
              max_is = IS.findMax is

neighbors :: (Int,Int,Int) -> [(Int,Int,Int)]
neighbors (x,y,z) = [(x+1,y,z),(x,y+1,z), (x,y,z+1),
                     (x-1,y,z), (x,y-1,z), (x,y,z-1)]

contains :: Cube -> (Int,Int,Int) -> Bool
contains c (x,y,z) |  Just ys <- c IM.!? x,
                      Just zs <- ys IM.!? y = z `IS.member` zs
contains _ _ = False

-- 3284 too high
-- 2029 too low
-- 2024 too low
-- 2018 too low

-- task2 :: [(Int, Int, Int)] -> Cube
task2 inp = (sides c, --sides (fully_contained_cube)
             length $ pos_neighbors,
             surface $ Set.toList pos_neighbors,
             length $ outside_neighbors,
             surface outside_neighbors
                             )
  where c = toCube inp
        surface :: [(Int,Int,Int)] -> Int
        surface = sum . map (\g -> length $ filter (contains c) $ neighbors g)
        pos_neighbors :: Set (Int,Int,Int)
        pos_neighbors = Set.fromList $
                concatMap (filter (not . contains c) .  neighbors) inp
        def_outside = (0,0,0)
        outside_neighbors = filter isOutside $ Set.toList pos_neighbors
        min_x = minimum (map (\(x,y,z) -> x) inp)
        max_x = maximum (map (\(x,y,z) -> x) inp)
        min_y = minimum (map (\(x,y,z) -> y) inp)
        max_y = maximum (map (\(x,y,z) -> y) inp)
        min_z = minimum (map (\(x,y,z) -> z) inp)
        max_z = maximum (map (\(x,y,z) -> z) inp)
        inRange :: (Int,Int,Int) -> Bool
        inRange (x,y,z) =  (min_x -1 <= x && x <= max_x +1)
                        && (min_y -1 <= y && y <= max_y +1)
                        && (min_z -1 <= z && z <= max_z +1)

        isOutside :: (Int,Int,Int) -> Bool
        isOutside p = isJust (bfs nf def_outside p)
            where nf cur_p = filter (not . contains c) $
                             filter inRange $
                             neighbors cur_p



main :: IO ()
main = do readInput "Day18/tinyExample" >>= print . task1
          readInput "Day18/example" >>= print . task1
          readInput "Day18/example" >>= print . task2
          readInput "Day18/input" >>= print . task1
          readInput "Day18/input" >>= print . task2


        --   print "te"
        --   print tinyExample
        --   print $ joined tinyExample
        --   print $ rotate tinyExample
        --   print $ joined $ rotate tinyExample
        --   print $ rotate $ rotate tinyExample
        --   print $ joined $ rotate $ rotate tinyExample
        --   print "ex"
        --   print $ 6* (length $ constituentCubes example)
        --   print example
        --   print $ joined example
        --   print $ rotate example
        --   print $ joined $ rotate example
        --   print $ rotate $ rotate example
        --   print $ joined $ rotate $ rotate example
        --   print $ sides example
        --   print $ sides tinyExample
        --   print $ sides tinyExample
