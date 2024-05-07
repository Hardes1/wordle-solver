module Printer.SearchMenuPrinter (printBackExtraInfo, printWelcomeMessage, printHelp, printFilteredWords) where

import Data.GameState (Color (..), WordDiff (..))
import Printer.WordDiffPrinter (printWordDiff, printWordDiffList)
import Data.Foldable (traverse_)

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

printFilteredWords :: [WordDiff] -> [String] -> IO ()
printFilteredWords wordDiffList wordList = do
  putStrLn "Current pattern set is:"
  printWordDiffList wordDiffList
  putStrLn $ "Words which satisfy pattern (total number of entries " <> show (length wordList) <> "):"
  traverse_ putStrLn wordList

printBackExtraInfo :: IO ()
printBackExtraInfo = putStrLn "You are leaving search menu!"