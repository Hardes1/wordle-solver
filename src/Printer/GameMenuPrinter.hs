module Printer.GameMenuPrinter(printHelp, printWelcomeMessage, printWordDiff, printGameResult, printHUD) where
import Data.GameState (WordDiff(..), Color (..), GameState (..), GameStatus (..),  IncorrectStatus(..))
import Data.Foldable (traverse_)
import Util.WordUtil (maxGuessSteps)

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

printWordDiffList :: [WordDiff] -> IO ()
printWordDiffList = traverse_ printWordDiff 

printGoingBackToMainMenu :: IO ()
printGoingBackToMainMenu = putStrLn "Going back to main menu..."

printProgress :: [WordDiff] -> IO ()
printProgress wordDiffList = do
     putStrLn "Your guesses: "
     printWordDiffList wordDiffList

printGameResult :: GameState -> IO ()
printGameResult env = case env of
        CorrectState status wordDiffList word -> do 
            putStrLn $ "You " <> show status <> "! Correct word was " <> word
            printProgress $ reverse wordDiffList
            printGoingBackToMainMenu
        IncorrectState Cancelled -> do 
            putStrLn "You cancelled the game!"
            printGoingBackToMainMenu

printHUD :: GameState -> IO ()
printHUD env = case env of
     CorrectState InProgress wordDiffList _ -> do
          putStrLn $ "Move: " <> show (length wordDiffList + 1) <> " / " <> show maxGuessSteps
     _ -> error "Illegal state"