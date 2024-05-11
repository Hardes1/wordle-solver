{-# LANGUAGE InstanceSigs #-}
module Parser.GameMenuCommandParser(parse, ParseError(..)) where
import Data.GameMenuCommand(Command(..))
import Util.WordUtil (isConsistOfLetters, isValidLength)
import qualified Data.WordError as WordError(ParseError(..))
import Util.ParseUtil (isCorrectWord)

data ParseError = WordParseError WordError.ParseError String | UnknownCommand String deriving (Eq)

instance Show ParseError where
    show :: ParseError -> String
    show (WordParseError err word) = "Parse error: " <> "Word '" <> word <> "'. " <> show err
    show (UnknownCommand cmd) =  "Unsupported command: '" <> cmd <> "'."

parse :: String -> Either ParseError Command
parse ":back" = Right Back
parse ":help" = Right Help
parse ":guess" = Right Guess
parse s@(':':_) = Left $ UnknownCommand s
parse word = do
    let result = isCorrectWord word
    case result of
        Left err -> Left $ WordParseError err word
        Right _ -> Right $ Word word
