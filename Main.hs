
import LibBlock
import System.Exit

main :: IO()
main = do
   putStrLn "Please input a 10-bit key: "
   initialKey <- getLine
   putStrLn "Please input an 8-bit message: "
   initialInput <- getLine
   let keyOne = generateKey1 (formatInput (initialKey))
   let keyTwo = generateKey2 (formatInput (initialKey))
   let inputFinal = (formatInput (initialInput))
   putStrLn "Your keys are Key One: "
   print keyOne
   putStrLn "\nKey Two: "
   print keyTwo
   putStrLn "\nWould you like to Encrypt('e') or Decrypt('d') or Quit ('q'): "
   choice <- getLine
   putStrLn "Your choice was: "
   print choice
   if (choice == "q")
      then exitSuccess
---else if (choice == "e")
------then print "encrypt"
---else if (choice == "d")
------then print "decrypt"
   else
      die "You f**ked up"
---fK initialInput keyOne KeyTwo choice

formatInput :: [Char] -> [Int]
formatInput x = map (read . (:"")) x :: [Int]
