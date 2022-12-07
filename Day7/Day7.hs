{-# LANGUAGE GHC2021 #-}
module Main where

import Text.ParserCombinators.ReadPrec ( lift, readPrec_to_S )
import Text.ParserCombinators.ReadP
import Control.Monad (void, replicateM)
import Data.Char (isDigit)
import Data.Maybe (mapMaybe)
import Debug.Trace
import Data.List
import Data.Function (on)


data DirTreeString = DT String [String]  [DirTreeString] deriving (Show)


data FSEntry = Dir String Int [FSEntry]
             | File String Int

instance Show FSEntry where
    show (Dir dname sz entries) =
         "- " ++ dname ++ " (dir, size=" ++ show sz ++ ")\n" ++ conts
         where conts = unlines $ map ("  " ++) $ lines $ unlines $ map show entries
    show (File fn sz) = "- " ++ fn ++ " (file, size=" ++ show sz ++ ")"

fsSize :: FSEntry -> Int
fsSize (File _ s) = s
fsSize (Dir _ s _)  = s

toFile :: String -> Maybe FSEntry
toFile ('d':'i':'r':_) = Nothing
toFile "" = Nothing
toFile str = Just $ File fn (read @Int fs)
    where (fs, _:fn) = span isDigit str

toFS :: DirTreeString -> FSEntry
toFS (DT dname ls_res sub_dirs) = Dir dname dsize (files ++ fs_sub_dirs)
   where files = mapMaybe toFile ls_res
         fs_sub_dirs = map toFS sub_dirs
         dsize = sum (map fsSize $ files ++ fs_sub_dirs)

instance Read DirTreeString where
    readsPrec = readPrec_to_S $ lift parse
      where parse = parseDir
            parseLine = manyTill get (choice [void $ char '\n',eof])
            lookCmd = do (('$':_):rest) <- lines <$> look
                         return ()
            parseLs = do _ <- string "$ ls"
                         manyTill parseLine (choice [lookCmd,eof])
            parseDir = do _ <- string "$ cd "
                          dir_name <- manyTill get (char '\n')
                          ls_res <- parseLs
                          d_res <- manyTill parseDir
                                     (choice [(void $ string "$ cd ..\n"),
                                              eof])
                          return (DT dir_name ls_res d_res)


task1 :: FSEntry -> Int
task1 (Dir _ sz entries) | sz >= 100_000 = sum (map task1 entries)
                         | otherwise = sz + (sum $ map task1 entries)
task1 _ = 0

flatten :: FSEntry -> [(String, Int)]
flatten d@(Dir n i entries) = (n,i):(concatMap flatten entries)
flatten _ = []

task2 :: FSEntry -> Int
task2 root@(Dir _ sz _) = snd $ minimumBy (compare `on` snd) $
                             filter ((>= req_size) . snd) $ flatten root
   where unused = 70_000_000 - sz
         req_size = 30_000_000 - unused

readInput :: FilePath -> IO DirTreeString
readInput = fmap (read @DirTreeString) . readFile
main :: IO ()
main = do readInput "example" >>= print . task1 . toFS
          readInput "input" >>= print . task1 . toFS
          readInput "example" >>= print . task2 . toFS
          readInput "input" >>= print . task2 . toFS