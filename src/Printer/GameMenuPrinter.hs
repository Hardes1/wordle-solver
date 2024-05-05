module Printer.GameMenuPrinter(printHelp) where

printHelp :: IO ()
printHelp = do
     putStrLn "<word> - input word to guess in wordle"
     putStrLn ":help - prints this menu again"
     putStrLn ":back - goes back to main menu"
     