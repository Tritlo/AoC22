module Main where

import Util
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (partition, sort)
import qualified Data.List as List
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Control.Monad (forM_)

data Input = I [(Int,Int)] deriving (Show)


instance Read Input where
    readsPrec = parsePrec parse
      where parse = I <$> (sepBy1 parsePair
                          (skipSpaces >> string "->" >> skipSpaces))
            parsePair :: ReadP (Int,Int)
            parsePair = do x <- readS_to_P (reads @Int)
                           char ','
                           y <- readS_to_P (reads @Int)
                           return (x,y)


readInput :: FilePath -> IO [Input]
readInput = fmap (map read  . lines) . readFile

-- betwixt :: (Int,Int) -> (Int,Int) -> (Int,Int) -> Bool
-- betwixt (x1,y1) (x2,y2) (x,y) =
--     if x1 == x2 && x == x1
--     then  (y1 <= y && y <= y2) || (y2 <= y && y <= y1)
--     else if y == y1
--          then  (x1 <= x && x <= x2) || (x2 <= x && x <= x1)
--          else False

-- blocked :: Input -> (Int,Int) -> Bool
-- blocked (I (p1:p2:r)) p = betwixt p1 p2 p || (blocked (I (p2:r)) p)
-- blocked (I _) x = False

type Coord = (Int,Int)
type Line = (Coord,Coord)

toLines :: [Input] -> [Line]
toLines = concatMap toLines'
  where toLines':: Input -> [(Coord,Coord)]
        toLines' (I (a:b:r)) = (a,b):(toLines' $ I (b:r))
        toLines' _ = []

isXLine :: Line -> Bool
isXLine ((x1,_), (x2,_)) = x1 == x2

toXMap :: Line -> IntMap [(Int,Int)]
toXMap l | not $ isXLine l = error "not an x line!!"
toXMap ((x,y1), (_,y2)) = IM.singleton x [(min y1 y2, max y1 y2)]

toYMap :: Line -> IntMap [(Int,Int)]
toYMap l | isXLine l = error "not a y line!!"
toYMap ((x1,y), (x2,_)) = IM.singleton y [(min x1 x2, max x1 x2)]

type CollMap = (IntMap [(Int,Int)], IntMap [(Int,Int)])



-- a LOT faster if we can BINARY search here!!
addGrain :: (Int,Int) -> CollMap -> CollMap
addGrain (x,y) (xl,yl) = (IM.alter (af y) x xl,
                          IM.alter (af x) y yl)
  where af x (Just l) = Just $ l''
           where p = (x,x)
                 l' = List.insert p l
                 Just i = List.elemIndex p l'
                 l'' = case splitAt (i-1) l' of
                         (f,(p0:p1:[])) ->
                             case merge p0 p1 of
                                 Just p3 -> f ++ [p3]
                                 _ -> l'
                         (f,(p0:p1:p2:r)) ->
                             case merge p0 p1 of
                                 Just p1' ->
                                     case merge p1' p2 of
                                         Just p3 -> f ++ (p3:r)
                                         _ -> f ++ (p1':p2:r)
                                 _ -> case merge p1 p2 of
                                         Just p2' -> f ++ (p0:p2':r)
                                         _ -> l'




        af i Nothing = Just [(i,i)]

merge :: (Int,Int) -> (Int,Int) -> Maybe (Int,Int)
merge (s1,e1) (s2,e2) = if abs (s2 - e1) <= 1
                        then Just (s1, e2)
                        else Nothing

collision :: CollMap -> (Int,Int) -> Bool
collision (xl,yl) (x,y) = xcoll || ycoll
   where ycoll = case xl IM.!? x of
                     Just xs -> any (\(y1,y2) -> y1 <= y && y <= y2) xs
                     Nothing -> False
         xcoll = case yl IM.!? y of
                     Just ys -> any (\(x1,x2) -> x1 <= x && x <= x2) ys
                     Nothing -> False

init_pos = (500,0)

grainFall :: (Int, CollMap) -> (Int,Int) -> (Int, CollMap)
grainFall v@(i, (_,y)) g@(_,gy) |
     Just (yMax,_) <- IM.lookupMax y, gy > yMax = v
grainFall v@(i, cm) g@(gx,gy) =
         if not (collision cm (gx,gy+1))
         then grainFall v (gx,gy+1)
         else if not (collision cm (gx-1,gy+1))
              then grainFall v (gx-1, gy+1)
              else if not (collision cm (gx+1,gy+1))
                   then grainFall v (gx+1, gy+1)
                   else let cm' = addGrain g cm
                        in grainFall (i+1,cm') init_pos

rangeSize :: (Int,Int) -> Int
rangeSize (s,e) = (e-s) + 1

task1 :: [Input] -> Int
task1 inps = fst $ grainFall (0, cmap) init_pos
    where (xs,ys) = partition isXLine $ toLines inps
          cmap = (fmap sort $ IM.unionsWith (++) $ map toXMap xs,
                  fmap sort $ IM.unionsWith (++) $ map toYMap ys)

task2 :: [Input] -> (Int, CollMap)
task2 inps = (floor, cmap)
   where getYs ((_,y1), (_,y2)) = [y1,y2]
         floor = 2 + (maximum $ concatMap getYs $ toLines inps)
         (xs,ys_orig) = partition isXLine $ toLines inps
         ys = ((minBound, floor), (maxBound,floor)):ys_orig
         cmap = (fmap sort $ IM.unionsWith (++) $ map toXMap xs,
                 fmap sort $ IM.unionsWith (++) $ map toYMap ys)


grainFall2 :: Int -> (Int, CollMap) -> (Int,Int) -> (Int, CollMap)
grainFall2 floor v@(i, cm) _ | collision cm init_pos = (i,cm)
grainFall2 floor v@(i, cm) g@(gx,gy) | gy+1 >= floor =
    let cm' = addGrain g cm
    in grainFall2 floor (i+1, cm') init_pos
grainFall2 floor v@(i, cm) g@(gx,gy) =
         if not (collision cm (gx,gy+1))
         then grainFall2 floor v (gx,gy+1)
         else if not (collision cm (gx-1,gy+1))
              then grainFall2 floor v (gx-1, gy+1)
              else if not (collision cm (gx+1,gy+1))
                   then grainFall2 floor v (gx+1, gy+1)
                   else let cm' = addGrain g cm
                        in grainFall2 floor (i+1,cm') init_pos
main :: IO ()
main = do
          readInput "Day14/example" >>= print . task1
          readInput "Day14/input" >>= print . task1
          (floor, cmap) <- task2 <$> readInput "Day14/example"
          let (r,gmap) = grainFall2 floor (0,cmap) init_pos
        --   readInput "Day14/input" >>= print . task1
          print r
          forM_ [0..11] $ \y ->
             do forM_ [485..515] $ \x -> do
                  if collision cmap (x,y)
                  then putStr "#"
                  else if collision gmap (x,y)
                       then putStr "O"
                       else putStr "."
                putStrLn ""
          (floor_i, cmap_i) <- task2 <$> readInput "Day14/input"
          let (r_i,gmap_i) = grainFall2 floor_i (0,cmap_i) init_pos
          print r_i