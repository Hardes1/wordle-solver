module Printer.GameMenuPrinter(printHelp, printWelcomeMessage) where

printHelp :: IO ()
printHelp = do
     putStrLn "<word> - input word to guess in wordle"
     putStrLn ":help - prints this menu again"
     putStrLn ":back - goes back to main menu"

printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the wordle game!"
