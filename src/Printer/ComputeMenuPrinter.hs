module Printer.ComputeMenuPrinter(printHelp, printDictionary) where
import Data.Foldable (traverse_)

printHelp :: IO ()
printHelp = do
    putStrLn "<word> - adds new word to the dictionary"
    putStrLn ":compute - finds a best word to start for a given set of words"
    putStrLn ":status - prints all words in the current set"
    putStrLn ":reset - clears dictionary"
    putStrLn ":help - prints this menu again"
    putStrLn ":back - goes back to the main menu"

printDictionary :: [String] -> IO ()
printDictionary dict = do
    putStrLn "Current set of words in the dictionary is:"
    traverse_ putStrLn dict