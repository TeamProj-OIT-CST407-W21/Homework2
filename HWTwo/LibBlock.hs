
module LibBlock where
import Data.List as L
import Data.Tuple as T


---- blocks needed for cipher

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

---functions to actively return elements from S0 and S1 (conversion helper needed to change binary 00, 01, 10, 11 into decimal values
---Haskell can use to access matrix elements

---returns list of 2 elements from sZERO
fromS0 :: [Int] -> [Int]
fromS0 list = sZERO !! (twoBitBinDec(list !! 0 : list !! 3 : [] )) !! (twoBitBinDec(list !! 1 : list !! 2 : [] )) 

---returns list of 2 elements from sONE
fromS1 :: [Int] -> [Int]
fromS1 list = sONE !! (twoBitBinDec(list !! 0 : list !! 3 : [] )) !! (twoBitBinDec(list !! 1 : list !! 2 : [] )) 


----basic 4x4 matrix foundation of S-Blocks, each with a 'address'/coordinate used to access
sZERO :: [[[Int]]]
sZERO = [[[0,1],[0,0],[1,1],[1,0]],[[1,1],[1,0],[0,1],[0,0]],[[0,0],[1,0],[0,1],[1,1]],[[1,1],[0,1],[1,1],[1,0]]]

sONE :: [[[Int]]]
sONE = [[[0,0],[0,1],[1,0],[1,1]],[[1,0],[0,0],[0,1],[1,1]],[[1,1],[0,0],[0,1],[0,0]],[[1,0],[0,1],[0,0],[1,1]]]


---- key generation functions
generateKey1:: [Int] -> [Int]
generateKey1 key = swapPEIGHT (L.concat [(circularShift (T.fst (L.splitAt 5 (swapPTEN key))) 1) , (circularShift (T.snd (L.splitAt 5 (swapPTEN key))) 1)])

generateKey2:: [Int] -> [Int]
generateKey2 key = swapPEIGHT (L.concat [(circularShift (T.fst (L.splitAt 5 (swapPTEN key))) 3) , (circularShift (T.snd (L.splitAt 5 (swapPTEN key))) 3)])

--- main encryption/decryption block
---naming of keyone, keytwo based on the block diagram where naming is based on encryption
fkComplete :: [Int] -> [Int] -> [Int] -> [Int]
fkComplete list keyone keytwo = swapIPINVERSE (L.concat [(fkTwo list keyone keytwo), (T.snd (L.splitAt 4 (fkMid list keyone)))])


---- SEQUENTIAL HELPERS FOR FKCOMPLETE
----naming conventions kept the same

--- everything except swapIPINVERSE
fkTwo :: [Int] -> [Int] -> [Int] -> [Int]
fkTwo list keyone keytwo = fkMidBranchFin (T.snd (L.splitAt 4 (fkMid list keyone))) (keytwo) (T.fst (L.splitAt 4 (fkMid list keyone)))

---- adds in SW block + low plain nibble but without second pass through
fkMid :: [Int] -> [Int] -> [Int]
fkMid list keyone = swapFORFK (L.concat [(fkOne list keyone), (T.snd (L.splitAt 4 (swapIP list)))])


----- despite name, will only output the first 4 high bits, needs to be concat with the proper 4 low
fkOne :: [Int] -> [Int] -> [Int]
fkOne list keyone = fkMidBranchFin (T.snd (L.splitAt 4 (swapIP list))) (keyone) (T.fst (L.splitAt 4 (swapIP list)))

---boxh is IP (swapIP) high nibble in fk1, SW (swaoFORFK) high nibble in fk2
---- extra layer of abstraction allows for core reuse
fkMidBranchFin :: [Int] -> [Int] -> [Int] -> [Int]
fkMidBranchFin list key boxh = arrXor (boxh) (swapPFOUR (fkMidBranch2 list key))

---- executes block diagram up until P4
fkMidBranch2 :: [Int] -> [Int] -> [Int]
fkMidBranch2 list key = L.concat [fromS0(T.fst(L.splitAt 4 (fkMidBranch1 list key))),fromS1(T.snd(L.splitAt 4 (fkMidBranch1 list key)))] 

--- begins the execution of the fk block, prioritized central datapath as is the most complex
---- the rest are added in gradually as more and more of the algorithm is completed
fkMidBranch1 :: [Int] -> [Int] -> [Int]
fkMidBranch1 list key = arrXor (key) (swapEP list)


---- LOGIC HELPERS AND RELATED

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

--- xor a 'bit list' of 1s and 0s formatted at [Int] 
arrXor :: [Int] -> [Int] -> [Int]
arrXor x y = L.zipWith oneBitXor x y

--- xor for a single bit
oneBitXor :: Int -> Int -> Int
oneBitXor x y 
    | (x == 1 && y == 1) = 0
    | (x == 0 && y == 0) = 0
    | (x == 0 && y == 1) = 1
    | (x == 1 && y == 0) = 1
    | (x < 0 || x > 1 || y < 0 || y > 1) = -1

--- CONVERSION HELPERS

---Data List indexes generally are left-most = first elem (unsure if architecture based or sequence based, assumed sequence based)
--- Example : [1, 0] reprenting binary '10' where 1 has index 0 and 0 has index of 1 in Haskell Lists
--- No relationship to mathematical 2^0, 2^1
----- Nothing bigger than 2 bits needed for it's purpose
twoBitBinDec :: [Int] -> Int
twoBitBinDec x = ((x !! 0) * 2) + ((x !! 1) * 1)