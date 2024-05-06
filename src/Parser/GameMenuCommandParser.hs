{-# LANGUAGE InstanceSigs #-}
module Parser.GameMenuCommandParser(parse, ParseError(..)) where
import Data.GameMenuCommand(Command(..))
import Util.WordUtil (isConsistOfLetters, isValidLength)

data ErrorType = InvalidLength | BadCharacter | UnknownCommand

data ParseError = ParseError ErrorType String

instance Show ParseError where
    show :: ParseError -> String
    show (ParseError InvalidLength word) = "Word should have length 5. Word '" <> word <> "' has length " <> show (length word) <> "."
    show (ParseError BadCharacter _) = "Word should only consist of alphabetical symbols."
    show (ParseError UnknownCommand cmd) =  "Unsupported command: '" <> cmd <> "'."

parse :: String -> Either ParseError Command
parse ":back" = Right Back
parse ":help" = Right Help
parse ":guess" = Right Guess
parse s@(':':_) = Left $ ParseError UnknownCommand s
parse word = do
    isCorrectWord word
    return $ Word word



isCorrectWord :: String -> Either ParseError ()
isCorrectWord word 
    | not $ isValidLength word = Left $ ParseError InvalidLength word
    | not $ isConsistOfLetters word = Left $ ParseError BadCharacter word
    | otherwise = return ()