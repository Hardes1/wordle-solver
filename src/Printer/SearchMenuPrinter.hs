module Printer.SearchMenuPrinter (printWelcomeMessage, printHelp) where

import Data.GameState (Color (..), WordDiff (..))
import Printer.WordDiffPrinter (printWordDiff)

printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the word searching by clue set!"

printSearchFormat :: IO ()
printSearchFormat = do
  putStrLn "<letter_sequence> <(R|G|Y)> - add 5-letter word to the clues"
  putStrLn "Example: command 'FrEsH RgyYG' will result in"
  printWordDiff $ WordDiff [(Red, 'F'), (Green, 'R'), (Yellow, 'E'), (Yellow, 'S'), (Green, 'H')]

printHelp :: IO ()
printHelp = do
  printSearchFormat
  putStrLn ":search - searches all words from la-dictionary with the clues in the state"
  putStrLn ":status - prints all clues which are in the state"
  putStrLn ":reset - clears all clues which are in the state"
  putStrLn ":help - prints this menu again"
  putStrLn ":back - goes back to the main menu"
