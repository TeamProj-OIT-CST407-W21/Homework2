module Main where

import LibBlock
import LibUI
import Data.List as L
import System.Exit

main :: IO()
main = do
   putStrLn "Please input a 10-bit key: "
   initialKey <- getLine
   if ((L.length (formatInput(initialKey))) /= 10)
      then die "Improper key length, killing program"
   else do
      if ((filterLessThanZero (formatInput(initialKey))) /= (L.length (formatInput(initialKey))))
         then die "Improper key, value less than zero"
      else if ((filterGreaterThanOne (formatInput(initialKey))) /= (L.length (formatInput(initialKey))))
         then die "Improper key, value greater than one"
      else do
         putStrLn "Please input an 8-bit message: "
         initialInput <- getLine
         let inputFinal = (formatInput (initialInput))
         if (L.length inputFinal /= 8)
            then die "Improper message length, killing program"
         else do
            if ((filterLessThanZero inputFinal) /= (L.length(inputFinal)))
               then die "Improper message, value less than zero"
            else if ((filterGreaterThanOne inputFinal) /= (L.length(inputFinal)))
               then die "Improper message, value greater than one"
            else do
               let keyOne = generateKey1 (formatInput (initialKey))
               let keyTwo = generateKey2 (formatInput (initialKey))
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
               else if (choice == "e")
                  then do
                     putStrLn ("Your encrypted message is: ")
                     print (fkComplete inputFinal keyOne keyTwo)
               else if (choice == "d")
                  then do
                     putStrLn ("Your decrypted message is: ")
                     print (fkComplete inputFinal keyTwo keyOne)
               else
                  die "Invalid choice, killing program"