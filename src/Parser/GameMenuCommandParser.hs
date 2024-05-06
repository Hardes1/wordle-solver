{-# LANGUAGE InstanceSigs #-}
module Parser.GameMenuCommandParser(parse, ParseError(..)) where
import Data.GameMenuCommand(Command(..))
import Data.Char (isAlpha)

data ParseError = InvalidLength String | BadCharacter | UnknownCommand String

instance Show ParseError where
    show :: ParseError -> String
    show (InvalidLength word) = "Word should have length 5. Word '" <> word <> "' has length " <> show (length word) <> "."
    show BadCharacter = "Word should only consist of alphabetical symbols."
    show (UnknownCommand cmd) =  "Unsupported command: '" <> cmd <> "'."

parse :: String -> Either ParseError Command
parse ":back" = Right Back
parse ":help" = Right Help
parse ":guess" = Right Guess
parse s@(':':_) = Left $ UnknownCommand s
parse word = do
    _ <- isCorrectWord word
    return $ Word word


isValidLength :: String -> Bool
isValidLength word = length word == 5

isConsistOfLetters :: String -> Bool
isConsistOfLetters = all isAlpha


isCorrectWord :: String -> Either ParseError ()
isCorrectWord word = do
    if not $ isValidLength word then Left (InvalidLength word)
    else if not $ isConsistOfLetters word then Left BadCharacter
    else Right ()