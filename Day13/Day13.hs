module Main where

import Text.ParserCombinators.ReadPrec (readPrec_to_S)
import qualified Text.ParserCombinators.ReadPrec as RP
import Text.ParserCombinators.ReadP
import Data.List (sort, findIndices)

import Util (chunks)

data Input = I Int
           | Li [Input] deriving (Show, Eq)

instance Read Input where
    readsPrec = readPrec_to_S (RP.lift parse)
      where parse = choice [I <$> readS_to_P (reads @Int),
                            parseLiInput]
            parseLiInput = do char '['
                              res <- sepBy parse (char ',')
                              char ']'
                              return (Li res)


readInput :: FilePath -> IO [(Input, Input)]
readInput = fmap (map f . chunks 3 . lines) . readFile
  where f :: [String]  -> (Input,Input)
        f (a:b:_) = (read a, read b)

instance Ord Input where
    compare (I l) (I r) = compare l r
    compare (Li []) (Li []) = EQ
    compare (Li []) (Li (_:_)) = LT
    compare (Li (_:_)) (Li []) = GT
    compare (Li (l:ls)) (Li (r:rs)) =
            case compare l r of
                EQ -> compare (Li ls) (Li rs)
                res -> res
    compare (Li li) i = compare (Li li) (Li [i])
    compare i (Li li) = compare (Li [i]) (Li li)



task1 :: [(Input, Input)] -> Int
task1 = sum . map fst . filter snd
        . zip [1..] . map ((==) LT . uncurry compare)

task2 :: [(Input,Input)] -> Int
task2 input = product $ map (+1) $
              findIndices (\i -> i == d2 || i == d6) sorted
  where d2 = Li [Li [I 2]]
        d6 = Li [Li [I 6]]
        sorted = (sort . (++ [d2,d6]) . concatMap (\(a,b) -> [a,b])) input

main :: IO ()
main = do
    readInput "Day13/example" >>= print . task1
    readInput "Day13/input" >>= print . task1
    readInput "Day13/example" >>= print . task2
    readInput "Day13/input" >>= print . task2