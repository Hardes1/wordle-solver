module Printer.CommonPrinter(printExit, printParseError) where

printExit :: IO ()
printExit = putStrLn "Going back to main menu..."

printParseError :: Show a => a -> IO ()
printParseError err = putStrLn $ "Parser error: " <> show err