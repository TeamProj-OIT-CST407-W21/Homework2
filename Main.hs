module Main where

import LibBlock as LB
import LibUI as UI
import Data.List as L
import System.Exit as E

main :: IO()
main = do
   putStrLn "Please input a 10-bit key: "
   initialKey <- getLine
   if ((L.length (UI.formatInput(initialKey))) /= 10)
      then E.die "Improper key length, killing program"
   else do
      if ((UI.filterLessThanZero (UI.formatInput(initialKey))) /= (L.length (UI.formatInput(initialKey))))
         then E.die "Improper key, value less than zero"
      else if ((UI.filterGreaterThanOne (UI.formatInput(initialKey))) /= (L.length (UI.formatInput(initialKey))))
         then E.die "Improper key, value greater than one"
      else do
         putStrLn "Please input an 8-bit message: "
         initialInput <- getLine
         let inputFinal = (UI.formatInput (initialInput))
         if (L.length inputFinal /= 8)
            then E.die "Improper message length, killing program"
         else do
            if ((UI.filterLessThanZero inputFinal) /= (L.length(inputFinal)))
               then E.die "Improper message, value less than zero"
            else if ((UI.filterGreaterThanOne inputFinal) /= (L.length(inputFinal)))
               then E.die "Improper message, value greater than one"
            else do
               let keyOne = LB.generateKey1 (UI.formatInput (initialKey))
               let keyTwo = LB.generateKey2 (UI.formatInput (initialKey))
               putStrLn "Your keys are Key One: "
               print keyOne
               putStrLn "\nKey Two: "
               print keyTwo
               putStrLn "\nWould you like to Encrypt('e') or Decrypt('d') or Quit ('q'): "
               choice <- getLine
               putStrLn "Your choice was: "
               print choice
               if (choice == "q")
                  then E.exitSuccess
               else if (choice == "e")
                  then do
                     putStrLn ("Your encrypted message is: ")
                     print (LB.fkComplete inputFinal keyOne keyTwo)
               else if (choice == "d")
                  then do
                     putStrLn ("Your decrypted message is: ")
                     print (LB.fkComplete inputFinal keyTwo keyOne)
               else
                  E.die "Invalid choice, killing program"
