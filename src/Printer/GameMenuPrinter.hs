module Printer.GameMenuPrinter(printHelp, printWelcomeMessage, printWordDiff) where
import Data.GameState (WordDiff(..), Color (..))
import Data.Foldable (traverse_)

printHelp :: IO ()
printHelp = do
     putStrLn "<word> - input word to guess in wordle"
     putStrLn ":help - prints this menu again"
     putStrLn ":back - goes back to main menu"

printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the wordle game!"

printWordDiff :: WordDiff -> IO ()
printWordDiff (WordDiff arr) = do
     traverse_ (\(color, chr) -> case color of
          Green -> putStr $ "\ESC[32m" <> [chr] <> "\ESC[0m"
          Yellow -> putStr $ "\ESC[33m" <> [chr] <> "\ESC[0m"
          Red -> putStr $ "\ESC[31m" <> [chr] <> "\ESC[0m") arr
     putStrLn []