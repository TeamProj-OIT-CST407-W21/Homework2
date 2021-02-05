module LibUI where

import Text.Read as R
import Data.List as L

formatInput :: [Char] -> [Int]
formatInput x = L.map (R.read . (:"")) x :: [Int]

filterInput :: [Char] -> [Char]
filterInput x = removeChar ' ' x

removeChar :: Char -> [Char] -> [Char]
removeChar character string = L.filter (\c -> c/=character) string
