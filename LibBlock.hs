
module LibBlock where
import Data.List as L

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
-- for right shift, the size of the final list will always be the same, therefore, can use the original length 
-- list length regardless to chop off the prepended part of the list
--- Note: with pure logical shift, left multiplies by 2 and right logical divides by 2
---- A circular shift does not perform this operation precisely, but was the inspiration behind the neg/pos convension
circularShift :: [Int] -> Int -> [Int]
circularShift list shift
     | shift < 0 = L.take (L.length list) (prependEnd list shift)
     | shift > 0 = L.drop shift (appendBegin list shift)
     | shift == 0 = list

--- takes the ending elements in a list and prepends them to the beginning
---- flexibility with passing in positive or negative shifts allows for flexibility with circularShfit
---- HOWEVER will ultimately do exactly the same operation (user can then disregard sign of shift, does same thing)
---- might be some case where being sign agnositic is a problem, in this context with this operation, within the limits
---- of forsight, seemed like a good idea here
prependEnd :: [Int] -> Int -> [Int]
prependEnd list shift 
   | (shift < 0) = L.concat [L.drop ((L.length list) + shift) list, list]
   | (shift > 0) = L.concat [L.drop ((L.length list) - shift) list, list]
   | (shift == 0) = list
   

--- takes the initial elements in a list and appends them to the end
--- in this case, can be of any specified number of elements, as long as does not exceed list length
appendBegin :: [Int] -> Int -> [Int]
appendBegin list shift = L.concat [list, L.take shift list]
