module LibUI where

import Text.Read as R
import Data.List as L

formatInput :: [Char] -> [Int]
formatInput x = L.map (R.read . (:"")) x :: [Int]

--filterInput :: [Char] -> [Char]
--filterInput x = removeChar ' ' x

--removeChar :: Char -> [Char] -> [Char]
--removeChar character string = L.filter (\c -> c/=character) string

filterLessThanZero :: [Int] -> Int
filterLessThanZero list = L.length(L.filter (-1<) list)

filterGreaterThanOne :: [Int] -> Int
filterGreaterThanOne list = L.length(L.filter (<2) list)
