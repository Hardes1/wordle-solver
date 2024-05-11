module Printer.GameMenuPrinter(printHelp, printGameStatus, printHUD) where
import Data.GameState (WordDiff(..), GameState (..), GameStatus (..),  IncorrectStatus(..))
import Printer.WordDiffPrinter(printWordDiffList)
import Util.WordUtil (maxGuessSteps)
import Printer.CommonPrinter(printExit)


printHelp :: IO ()
printHelp = do
     putStrLn "<word> - input word to guess in wordle"
     putStrLn ":guess - shows current guesses"
     putStrLn ":help - prints this menu again"
     putStrLn ":back - goes back to main menu"

printProgress :: [WordDiff] -> IO ()
printProgress wordDiffList = do
     putStrLn "Your guesses: "
     printWordDiffList wordDiffList

printGameStatus :: GameState -> IO ()
printGameStatus env = case env of
        CorrectState InProgress wordDiffList _ -> do
            putStrLn $ "The game is " <> show InProgress <> "."
            if not $ null wordDiffList then printProgress $ reverse wordDiffList else putStrLn "You didn't do any guess yet."
        CorrectState status wordDiffList word -> do 
            putStrLn $ "You " <> show status <> "! Correct word was " <> word <> "."
            printProgress $ reverse wordDiffList
            printExit
        IncorrectState Cancelled -> do 
            putStrLn "You cancelled the game!"
            printExit

printHUD :: GameState -> IO ()
printHUD env = case env of
     CorrectState InProgress wordDiffList _ -> do
          putStrLn $ "Move: " <> show (length wordDiffList + 1) <> " / " <> show maxGuessSteps
     _ -> error "Illegal state"