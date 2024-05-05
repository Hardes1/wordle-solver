module ConsolePrintUtil(printWelcomeMessage, printHelp, printExit) where

printWelcomeMessage :: IO ()
printWelcomeMessage = putStrLn "Welcome to the wordle-solver!"

printHelp :: IO ()
printHelp = do
    putStrLn ":game - starts a new world game"
    putStrLn ":search - searchs a set of words based on clues"
    putStrLn ":compute - computes the best words for a given set of 5-letter words"
    putStrLn ":help - prints this menu again"
    putStrLn ":quit - exits the program"

printExit :: IO ()
printExit = putStrLn "Thank you for using my program!"