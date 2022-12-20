{-# LANGUAGE  GHC2021 #-}
{-# LANGUAGE  RecordWildCards #-}
{-# LANGUAGE OverloadedRecordDot #-}
module Main where

import Util
import Data.List (sortBy)
import Data.Function (on)
import Data.Set (Set)
import qualified Data.Set as Set



type Quple = (Int,Int,Int,Int)

indexQ :: Quple -> Int -> Int
indexQ (a,_,_,_) 0 = a
indexQ (_,b,_,_) 1 = b
indexQ (_,_,c,_) 2 = c
indexQ (_,_,_,d) 3 = d

data Input = Bl {or  :: Quple,
                 cr  :: Quple,
                 obr :: Quple,
                 ger :: Quple} deriving (Show)

instance Read Input where
    readsPrec = parsePrec (parse)
      where parse =
                do string "Blueprint"
                   id <- readS_to_P (reads @Int)
                   char ':'
                   skipSpaces
                   string "Each ore robot costs"
                   skipSpaces
                   orc <- readS_to_P (reads @Int)
                   string " ore."
                   skipSpaces
                   string "Each clay robot costs"
                   skipSpaces
                   crc <- readS_to_P (reads @Int)
                   string " ore."
                   skipSpaces
                   string "Each obsidian robot costs"
                   skipSpaces
                   obroc <- readS_to_P (reads @Int)
                   string " ore and "
                   obrcc <- readS_to_P (reads @Int)
                   string " clay."
                   skipSpaces
                   string "Each geode robot costs"
                   skipSpaces
                   groc <- readS_to_P (reads @Int)
                   string " ore and "
                   grcc <- readS_to_P (reads @Int)
                   string " obsidian."
                   return  $ Bl{ or=(orc,0,0,0),
                                 cr=(crc,0,0,0),
                                 obr=(obroc,obrcc,0,0),
                                 ger=(groc,0,grcc,0)}




data State = RState { robots  :: Quple, -- ore, clay, obsidian, geode
                      stocks  :: Quple,
                      factory :: Quple } deriving (Show, Eq, Ord)

emptyQuple :: Quple
emptyQuple = (0,0,0,0)
mining :: State -> State

mining rs@(RState {..}) = rs'
  where rs' = rs{stocks=addQuple stocks robots,
                 robots=addQuple robots factory,
                 factory=emptyQuple}

addQuple :: Quple -> Quple -> Quple
addQuple (a,b,c,d) (x,y,z,w) = (a+x, b+y, c+z, d+w)
negateQuple :: Quple -> Quple
negateQuple (a,b,c,d) = (-a, -b,-c,-d)

data Action = Pass
            | BuildOre
            | BuildClay
            | BuildObs
            | BuildGeo deriving (Eq, Show, Ord, Enum)

takeAction :: Input -> State -> Int -> Action -> State
takeAction bl rs@(RState {..}) _ BuildOre
        = rs {factory = (1,0,0,0),
              stocks = stocks `addQuple` (negateQuple bl.or)}
takeAction bl rs@(RState {..}) _ BuildClay
        = rs {factory = (0,1,0,0),
              stocks = stocks `addQuple` (negateQuple bl.cr)}
takeAction bl rs@(RState {..}) _ BuildObs
        = rs {factory = (0,0,1,0),
              stocks = stocks `addQuple` (negateQuple bl.obr)}
takeAction bl rs@(RState {..}) t BuildGeo
        = rs {factory = (0,0,0,0),
              stocks =  (0,0,0,t-1) `addQuple`
                        (stocks `addQuple`
                      (negateQuple bl.ger))}
takeAction _ rs _ Pass = rs





readInput :: FilePath -> IO [Input]
readInput = fmap (map read . lines) . readFile

exActs :: [Action]
exActs = [Pass,
          Pass,
          BuildClay,
          Pass,
          BuildClay,
          Pass,
          BuildClay,
          Pass,
          Pass,
          Pass,
          BuildObs,
          BuildClay,
          Pass,
          Pass,
          BuildObs,
          Pass,
          Pass,
          BuildGeo,
          Pass,
          Pass,
          BuildGeo,
          Pass,
          Pass,
          Pass]

runRound :: Input -> State -> Int -> Action -> State
runRound bl st t act = mining $ takeAction bl st t act

initialState :: State
initialState = RState {stocks = emptyQuple,
                      factory= emptyQuple,
                      robots = (1,0,0,0) }

costOf :: Input -> Action -> Quple
costOf _ Pass = emptyQuple
costOf bl BuildOre = bl.or
costOf bl BuildClay = bl.cr
costOf bl BuildObs = bl.obr
costOf bl BuildGeo = bl.ger

isPayable :: Quple -> Quple -> Bool
isPayable (a,b,c,d) (x,y,z,w) = (a >= x &&
                                 b >= y &&
                                 c >= z &&
                                 d >= w)

possibleActions :: Input -> State -> [Action]
possibleActions bl st =
        if (isPayable st.stocks $ costOf bl BuildGeo)
        then [BuildGeo] else
        filter (isPayable st.stocks . costOf bl) $ [(Pass :: Action)..]

task1 :: [Input] -> Int
task1 = sum . zipWith (\res ind -> ind*res) [1..] .  taskN 24

task2 :: [Input] -> Int
task2 = product . taskN 32 . take 3

taskN :: Int -> [Input] -> [Int]
taskN time = map (flip task1' 0)
  where
    task1' :: Input -> Int -> Int
    task1' bl ind = go 0
        where (m_o:m_c:m_b:[]) = map (\i -> maximum (map (flip indexQ i)
                                      [bl.or, bl.obr, bl.ger,bl.cr])) [0..2]
              ndfs (n,_) | n >= time = []
              ndfs (n,st) = map (\p -> (n+1, runRound bl st (time-n) p)) pos'
                  where pos = reverse $ possibleActions bl st
                        pos' = h n st pos
                        h _ _ [] = []
                        h n st@(RState{stocks=(o,c,b,g),
                                    robots=(or,cr,br,_)}) (a:acs)
                                    | BuildOre <- a =
                                        if or >= m_o
                                        || or*t + o >= t* m_o
                                        || br >= 1
                                        then rest else (BuildOre:rest)
                                    | BuildClay <- a =
                                        if cr >= m_c
                                        || cr*t + c >= t*m_c
                                        || g >= 1
                                        then rest else (BuildClay:rest)
                                    | BuildObs <- a =
                                        if br >= m_b
                                        || br*t + b >= t*m_b

                                        then rest else (BuildObs:rest)
                                    | otherwise = a:rest
                            where rest = h n st acs
                                  t = time - n
              go n = case dfs ndfs (0,initialState)
                      (\(_,RState{stocks=(_,_,_,g)}) -> g >= n) of
                          Nothing -> traceShow ("found!", ind, (n-1)) (n-1)
                          Just p | (_,RState{stocks=(_,_,_,fg)}) <- last p
                                    -> traceShow (ind, n, fg) $ go (fg+1)


main :: IO ()
main = do
        readInput "Day19/example" >>= print
        readInput "Day19/example" >>= print . task1
        readInput "Day19/input" >>= print . task1
        readInput "Day19/example" >>= print . task2
        readInput "Day19/input" >>= print . task2
        -- #1 was 21
        -- #2 was 7
        -- #3 was 39

