module Printer.ComputeMenuPrinter(printHelp, printDictionary, printComputeResult, printNewWord) where
import Data.Foldable (traverse_)
import Data.GameState (WordDiff)
import Printer.WordDiffPrinter (printWordDiffList)
import Data.Char (toUpper)

printHelp :: IO ()
printHelp = do
    putStrLn "<word> - adds new word to the dictionary"
    putStrLn ":compute - finds a best word to start for a given set of words"
    putStrLn ":status - prints all words in the current set"
    putStrLn ":reset - clears dictionary"
    putStrLn ":help - prints this menu again"
    putStrLn ":back - goes back to the main menu"

printComputeResult :: String -> [WordDiff] -> IO ()
printComputeResult bestWord wordDiffList = do
    putStrLn $ "The best word is " <> bestWord <> "."
    putStrLn "With the following matches: "
    printWordDiffList wordDiffList

printDictionary :: [String] -> IO ()
printDictionary dict = do
    putStrLn "Current set of words in the dictionary is:"
    traverse_ putStrLn dict

printNewWord :: String -> IO ()
printNewWord word = do
    putStrLn $ "New word was added to the dictionary: '" <> map toUpper word <> "'."