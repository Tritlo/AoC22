module Util (chunks,
             module Text.ParserCombinators.ReadP,
             parsePrec) where

import Text.ParserCombinators.ReadP
import Text.ParserCombinators.ReadPrec
import qualified Text.ParserCombinators.ReadPrec as RP


parsePrec :: ReadP a -> Int -> ReadS a
parsePrec parse = readPrec_to_S (RP.lift parse)

chunks :: Int -> [a] -> [[a]]
chunks 0 _ = []
chunks n xs | length xs <= n = [xs]
            | otherwise = let (s,r) = splitAt n xs
                          in s : chunks n r