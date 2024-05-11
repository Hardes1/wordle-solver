module Printer.CommonPrinter(printExit, printParseError, printBackExtraInfo, printWelcomeMessage) where

printWelcomeMessage ::  String -> IO ()
printWelcomeMessage place = putStrLn $ "Welcome to the " <> place <> "!"

printExit :: IO ()
printExit = putStrLn "Going back to main menu..."

printParseError :: Show a => a -> IO ()
printParseError err = putStrLn $ "Parser error: " <> show err

printBackExtraInfo :: String -> IO ()
printBackExtraInfo place = putStrLn $ "You are leaving " <> place <> " menu!"