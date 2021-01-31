
module LibBlock where

swapIP :: [Int] -> [Int]
swapIP x = x !! 1 : x !! 5 : x !! 2 : x !! 0 : x !! 3 : x !! 7 : x !! 4 : x !! 6 : []

swapIPINVERSE ::[Int] -> [Int]
swapIPINVERSE x = x !! 3 : x !! 0 : x !! 2 : x !! 4 : x !! 6 : x !! 1 : x !! 7 : x !! 5 : []

swapPTEN :: [Int] -> [Int]
swapPTEN x = x !! 2 : x !! 4 : x !! 1 : x !! 6 : x !! 3 : x !! 9 : x !! 0 : x !! 8 : x !! 7 : x !! 5 : []

swapPEIGHT :: [Int] -> [Int]
swapPEIGHT x = x !! 5 : x !! 2 : x !! 6 : x !! 3 : x !! 7 : x !! 4 : x !! 9 : x !! 8 : []

swapEP :: [Int] -> [Int]
swapEP x = x !! 3 : x !! 0 : x !! 1 : x !! 2 :x !! 1 : x !! 2 : x !! 3 : x !! 0 : []
---- might also use circular shift?

swapPFOUR :: [Int] -> [Int]
swapPFOUR x = x !! 1 : x !! 3 : x !! 2 : x !! 0 : []

---- Performs the function of the SW box
---- Note, could also use splitAt and concat
swapFORFK :: [Int] -> [Int]
swapFORFK x = x !! 4 : x !! 5 : x !! 6 : x !! 7 : x !! 0 : x !! 1 : x !! 2 : x !! 3 : []

sZERO :: [[Int]]
sZERO = [[1,0,3,2],[3,2,1,0],[0,2,1,3],[3,1,3,2]]

sONE :: [[Int]]
sONE = [[0,1,2,3],[2,0,1,3],[3,0,1,0],[2,1,0,3]]
--- remember to change numbers to binary form, will become [[[Int]]]
---    using x!!0 x!!1 to reference cells to retrieve bits from



---keyGeneration :: [Int] -> ([Int], [Int])
---keyGeneration x = do
---   let keyInitial = swapPTEN x

---fK :: [Int] -> [Int] -> [Int]
---fK x key = do
---   let xs = splitAt 4 x
---   let leftHalf = fst xs
---   let rightHalf = snd xs
---   let rightHalfModified = fInner (snd xs) key
---   let leftHalfModified = leftHalf xor rightHalfModified
---   let y = leftHalfModified ++ rightHalf
---   return y

--- bound == list original size
--- normally R shift would be length - 1 EXCEPT new list is + 1 element, + 1 - 1 negate eachother
---- therefore, just using list length works just fine
---- passing in a negative number will give right circular, positive will give left circular
circularShift :: [Int] -> Int -> [Int]
circularShift list shift
     | shift < 0 = take (length list) (prependLast list)
     | shift > 0 = drop 1 (appendFirst list)
     | shift == 0 = list


prependLast :: [Int] -> [Int]
prependLast list = concat [[last list], list]

appendFirst :: [Int] -> [Int]
appendFirst list = concat [list, [head list]]

