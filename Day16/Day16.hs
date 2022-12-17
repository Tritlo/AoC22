{-# LANGUAGE  GHC2021, RecordWildCards, OverloadedRecordDot #-}
module Main where

import Util
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import qualified Data.Set as Set
import Data.Function (on)
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Input = V {key :: String,
                fr :: Int,
                tunnels :: [String]} deriving (Show)

instance Read Input where
    readsPrec  = parsePrec parse
      where parse = do string "Valve"
                       skipSpaces
                       key <- count 2 get
                       skipSpaces
                       string "has flow rate="
                       fr <- readS_to_P reads
                       string "; tunnel"
                       optional (char 's')
                       string " lead"
                       optional (char 's')
                       string " to valve"
                       optional (char 's')
                       skipSpaces
                       tunnels <- sepBy1 (count 2 get) (string ", ")
                       return $ V{..}





readInput :: FilePath -> IO [Input]
readInput = lineInput

type TunnelMap = Map String (Map String Int)
type FlowRates = Map String Int

initialMap :: [Input] -> (TunnelMap, FlowRates)
initialMap inp = (Map.unions tms, Map.unions frs)
  where toPair (V k fr t) = (Map.singleton k $ Map.fromList $ zip t (repeat 1),
                             Map.singleton k fr)
        (tms, frs) = unzip $ map toPair inp


updateMap :: TunnelMap -> TunnelMap
updateMap mp = Map.mapWithKey upd mp
  where upd ok ts = Map.fromList $ concatMap f vs
          where vs = Map.assocs ts
                f (k,d) =(k,d):( filter (not . flip Map.member ts . fst ) $
                                 filter ((/= ok) . fst) $
                                 map (\(k',d') -> (k',d'+d))
                                 $ Map.assocs (mp Map.! k))



fixMap :: TunnelMap -> TunnelMap
fixMap cur_map | um /= cur_map = fixMap um
    where um = updateMap cur_map
fixMap m = m

simplify :: String -> FlowRates -> TunnelMap -> TunnelMap
simplify initial_loc fr tm = Map.fromList (map (\(k, els) -> (k, Map.fromList $
                                                                 filter (isOk' . fst) $
                                                                 Map.assocs els)) assocs)
    where assocs = filter (isOk . fst) $ Map.assocs tm
          isOk k | k == initial_loc = True
          isOk k = isOk' k
          isOk' k = (fr Map.! k) > 0


-- task1 :: [Input] -> Map String (Map String (Int,Int))
task1 :: [Input] -> Int
task1 inps =  calcTotal 30 "AA" $ List.maximumBy (compare `on` (calcTotal 30 "AA"))
             $ calcOrder [] 30 "AA"
    where (tm, frm) = initialMap inps
          fixed = fixMap tm
          add_fr = Map.mapWithKey (\k d -> (d, frm Map.! k))
          simple = simplify "AA" frm fixed
          calcTotal cur_time cl _ | cur_time <= 0 = 0
          calcTotal cur_time cl [] = 0
          calcTotal cur_time cl (k:ks) =
             let lk = w_frs Map.! cl
                 (d,fr) = lk Map.! k
             in fr*(cur_time - 1 - d) + calcTotal (cur_time - d - 1) k ks


          w_frs = Map.map add_fr simple
          calcOrder :: [String] -> Int -> String -> [[String]]
          calcOrder visited cur_time cur_loc =
            case mx of
                [] -> [reverse visited]
                _ -> concatMap next mx
            where ns :: [(String, (Int,Int))]
                  ns = Map.assocs $
                       Map.filter (\(d,_) -> cur_time - d > 0) $
                       flip Map.withoutKeys v_set $ (w_frs Map.! cur_loc)
                  mx :: [(String, (Int,Int))]
                  mx = List.sortBy (compare `on` (\(_,(_,g)) -> g)) ns
                  v_set = Set.fromList visited
                  next :: (String, (Int,Int)) -> [[String]]
                  next (k',(d',_)) = calcOrder (k':visited)
                                               (cur_time - d' - 1) k'


task2 :: [Input] -> Int
task2 inps = pcomp max_path
    where (tm, frm) = initialMap inps
          max_path = List.maximumBy (compare `on` pcomp) paths
          pcomp (p1, p2) = calcTotal 26 "AA" p1 + calcTotal 26 "AA" p2
          paths = calcOrder Set.empty (IM.fromList [(0,[]), (1,[])])
                                          (IM.fromList [(0,26),(1,26)])
                                          (IM.fromList [(0,"AA"),(1,"AA")])
          fixed = fixMap tm
          add_fr = Map.mapWithKey (\k d -> (d, frm Map.! k))
          simple = simplify "AA" frm fixed
          calcTotal cur_time cl _ | cur_time <= 0 = 0
          calcTotal cur_time cl [] = 0
          calcTotal cur_time cl (k:ks) =
             let lk = w_frs Map.! cl
                 (d,fr) = lk Map.! k
             in fr*(cur_time - 1 - d) + calcTotal (cur_time - d - 1) k ks

          w_frs = Map.map add_fr simple
          calcOrder :: Set.Set String ->
                       IntMap [String] ->
                       IntMap Int ->
                       IntMap String ->
                       [([String],[String])]
          calcOrder v_set paths times locs =
            case mx of
                [] -> [(reverse $ paths IM.! 0,
                        reverse $ paths IM.! 1)]
                _ -> concatMap next mx
            where
                  [(_,my_time), (_,el_time)] = IM.assocs times
                  ind = if my_time >= el_time then 0 else 1
                  ns :: [(String, (Int,Int))]
                  ns = Map.assocs $
                       Map.filter (\(d,_) -> times IM.! ind - d > 0) $
                       flip Map.withoutKeys v_set $ (w_frs Map.! (locs IM.! ind))
                  mx :: [(String, (Int,Int))]
                  mx = List.sortBy (compare `on` (\(_,(_,g)) -> g)) ns
                  next :: (String, (Int,Int)) -> [([String], [String])]
                  next (k',(d',_)) =
                    calcOrder (k' `Set.insert` v_set)
                               (IM.adjust (\p -> (k':p)) ind paths)
                               (IM.adjust (\i -> i - d' - 1) ind times)
                               (IM.adjust (\_ -> k') ind locs)






main :: IO ()
main = do ex@(v:_) <- readInput "Day16/example"
          mapM_ print ex
          let (tm, frm) = initialMap ex
              fixed = fixMap tm
        --   mapM_ print $ Map.assocs $ simplify (v.key) frm fixed
        --   mapM_ print $ Map.assocs $ task1 ex
          print $ task1 ex
          putStrLn ""
        --   readInput "Day16/input" >>= mapM_ print . Map.assocs . task1

          readInput "Day16/input" >>= print . task1
          print $ task2 ex
          readInput "Day16/input" >>= print . task2