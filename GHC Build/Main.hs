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
      then do
       putStrLn "ERROR: Improper key length!  Returning to Top!"
       main
   else do
      if ((UI.filterLessThanZero (UI.formatInput(initialKey))) /= (L.length (UI.formatInput(initialKey))))
         then do
           putStrLn "ERROR: Improper key, value less than zero!  Returning to Top!"
           main
      else if ((UI.filterGreaterThanOne (UI.formatInput(initialKey))) /= (L.length (UI.formatInput(initialKey))))
         then do
           putStrLn "ERROR: Improper key, value greater than one! Returning to Top!"
           main
      else do
         putStrLn "Please input an 8-bit message: "
         initialInput <- getLine
         let inputFinal = (UI.formatInput (initialInput))
         if (L.length inputFinal /= 8)
            then do
              putStrLn "ERROR: Improper message length! Returning to Top!"
              main
         else do
            if ((UI.filterLessThanZero inputFinal) /= (L.length(inputFinal)))
               then do
                 putStrLn "ERROR: Improper message, value less than zero! Returning to Top!"
                 main
            else if ((UI.filterGreaterThanOne inputFinal) /= (L.length(inputFinal)))
               then do
                  putStrLn "ERROR: Improper message, value greater than one! Returning to Top!"
                  main
            else do
               let keyOne = LB.generateKey1 (UI.formatInput (initialKey))
               let keyTwo = LB.generateKey2 (UI.formatInput (initialKey))
               putStrLn "Your keys are:\n" 
               putStrLn "Key One: "
               print keyOne
               putStrLn "\nKey Two: "
               print keyTwo
               putStrLn "\nWould you like to Encrypt('e') or Decrypt('d'): "
               choice <- getLine
               putStrLn "Your choice was: "
               print choice
               if (choice == "e")
                  then do
                     putStrLn ("Your encrypted message is: ")
                     print (fkComplete inputFinal keyOne keyTwo)
                     putStrLn "\nPress y to continue, or any other key to exit: "
                     continue <-getLine
                     if (continue == "y")
                         then main
                     else
                         E.exitSuccess
               else if (choice == "d")
                  then do
                     putStrLn ("Your decrypted message is: ")
                     print (fkComplete inputFinal keyTwo keyOne)
                     putStrLn "\nPress y to continue, or any other key to exit: "
                     continue <-getLine
                     if (continue == "y")
                         then main
                     else
                         E.exitSuccess
               else do
                  putStrLn "ERROR: Invalid choice!  Returning to Top!"
                  main
