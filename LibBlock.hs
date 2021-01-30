
module LibBlock where

swapIP :: [int] -> [int]
swapIP x = x !! 2 : x !! 6 : x !! 3 : x !! 1 : x !! 4 : x !! 8 : x !! 5 : x !! 7 : []

swapIPINVERSE ::[int] -> [int]
swapIPINVERSE x = x !! 4 : x !! 1 : x !! 3 : x !! 5 : x !! 7 : x !! 2 : x !! 8 : x !! 6 : []

swapPTEN :: [int] -> [int]
swapPTEN x = x !! 3 : x !! 5 : x !! 2 : x !! 7 : x !! 4 : x !! 10 : x !! 1 : x !! 9 : x !! 8 : x !! 6 : []

swapPEIGHT :: [int] -> [int]
swapPEIGHT x = x !! 6 : x !! 3 : x !! 7 : x !! 4 : x !! 8 : x !! 5 : x !! 10 : x !! 9 : []
--- might get mad because it is going to have a length 10 list and return a length 8 list?

swapEP :: [int] -> [int]
swapEP x = x !! 4 : x !! 1 : x !! 2 : x !! 3 :x !! 2 : x !! 3 : x !! 4 : x !! 1 : []
---- might also use circular shift?

swapPFOUR :: [int] -> [int]
swapPFOUR x = x !! 2 : x !! 4 : x !! 3 : x !! 1 : []

swapFORFK :: [int] -> [int]
swapFORFK x = x !! 4 : x !! 5 : x !! 6 : x !! 7 : x !! 0 : x !! 1 : x !! 2 : x !! 3 : []

sZERO :: [[int]]
sZERO = [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]

sONE :: [[int]]
sONE = [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]

fK :: [int] -> [int]
fK x = do
   let xs = splitAt 4 x
   let leftHalf = fst xs
   let rightHalf = snd xs
   let rightHalfModified = fInner (snd xs)
   let leftHalfModified = leftHalf xor rightHalfModified
   x = leftHalfModified ++ rightHalf

--- bound == list original size
--- normally R shift would be length - 1 EXCEPT new list is + 1 element, + 1 - 1 negate eachother
---- therefore, just using list length works just fine
circularShift :: [Int] -> Int -> [Int]
circularShift list shift
     | shift < 0 = take (length list) (prependLast list)
     | shift > 0 = drop 1 (appendFirst list)
     | shift == 0 = list


prependLast :: [Int] -> [Int]
prependLast list = concat [[last list], list]

appendFirst :: [Int] -> [Int]
appendFirst list = concat [list, [head list]]

