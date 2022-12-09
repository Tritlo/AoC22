module Main where
import qualified Data.Set as Set
import Data.Set (Set)
import Debug.Trace

data Movement = U Int
              | R Int
              | L Int
              | D Int deriving (Read, Show)

moves :: Movement -> Int
moves (U n) = n
moves (R n) = n
moves (L n) = n
moves (D n) = n

readInput :: FilePath -> IO [Movement]
readInput = fmap (map read . lines) . readFile

dist :: (Int,Int) -> (Int,Int) -> Double
dist (x1,y1) (x2,y2)  = sqrt $ xdiff*xdiff + ydiff*ydiff
        where xdiff = fromIntegral $ x1 - x2
              ydiff = fromIntegral $ y1 - y2


type State = ((Int,Int), (Int,Int), Set (Int,Int))


tailMove :: (Int,Int) -> (Int,Int) -> Bool
tailMove h' t = dist h' t >= 2

headChange :: Movement -> (Int,Int) -> (Movement, (Int,Int))
headChange mv st | moves mv == 0 = (mv, st)
headChange (U n) (x,y) = (U (n-1), (x,y+1))
headChange (D n) (x,y) = (D (n-1), (x,y-1))
headChange (L n) (x,y) = (L (n-1), (x-1,y))
headChange (R n) (x,y) = (R (n-1), (x+1,y))

doMove :: Movement -> State -> State
doMove mv st | moves mv == 0 = st
doMove mv (h,t,visited) = doMove mv' $ if tailMove h' t
                                       then  (h', h, Set.insert h visited)
                                       else  (h', t, visited)
    where (mv', h') = headChange mv h

doMoves :: Movement -> (Int,Int) -> [(Int, Int)] -> ((Int,Int), [(Int,Int)])
doMoves mv h tpos | moves mv == 0 = (h, tpos)
doMoves mv h ts = doMoves mv' h' $ doChangeOnly h' ts
  where (mv', h') = headChange mv h

doChangeOnly :: (Int,Int) -> [(Int,Int)] -> [(Int,Int)]
doChangeOnly h' ts@(t:r) = if tailMove h' t
                           then let (hx,hy) = h'
                                    (tx,ty) = t
                                    t' = (tx+xdiff,ty+ydiff)
                                    xdiff = signum (hx - tx)
                                    ydiff = signum (hy - ty)
                                in (t':ts)
                           else ts

runMoves :: [Movement] -> ((Int,Int), (Int,Int), Set (Int, Int))
runMoves = foldl (flip doMove) ((0,0),(0,0), Set.singleton (0,0))

task1 :: [Movement] -> Int
task1 = Set.size . third . runMoves

third :: (a,b,c) -> c
third (a,b,c) = c

tailMoves :: [Movement] -> [(Int,Int)]
tailMoves = reverse . snd . foldl (\(h,ts) a -> doMoves a h ts) ((0,0),[(0,0)])

knotMoves :: [(Int,Int)] -> [(Int,Int)]
knotMoves = reverse . foldl (flip doChangeOnly) [(0,0)]

task2 :: [Movement] -> Int
task2 = Set.size . Set.fromList . last . take 9 . iterate knotMoves . tailMoves

main :: IO ()
main = do readInput "input" >>= print . Set.size .
                                Set.fromList . tailMoves
          exm <- readInput "example"
          let tms = tailMoves exm
          exm2 <- readInput "example-large"
          print $ Set.size $ Set.fromList $ last
                          $ take 9 $ iterate knotMoves $ tailMoves exm2
          readInput "input" >>= print . task2

-- ......
-- ......
-- ......
-- ....H.
-- 4321..

-- ......
-- ......
-- ....H.
-- ....1.
-- 432_|.

-- is

-- ......
-- ......
-- ....H.
-- .4321.
-- s.....

-- why not
-- ......
-- ......
-- ....H.
-- ....1.
-- s432..

