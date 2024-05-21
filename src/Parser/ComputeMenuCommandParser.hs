{-# LANGUAGE InstanceSigs #-}
module Parser.ComputeMenuCommandParser(parse, ParseError(..)) where
import Data.ComputeMenuCommand (Command (..))
import qualified Data.WordError as WordError(ParseError(..))
import Util.ParseUtil (isCorrectWord)

data ParseError = UnknownCommand String | WordParseError WordError.ParseError String deriving Eq

instance Show ParseError where
    show :: ParseError -> String
    show (WordParseError err word) =  "Word '" <> word <> "'. " <> show err
    show (UnknownCommand cmd) =  "Unsupported command: '" <> cmd <> "'."

parse :: String -> Either ParseError Command
parse ":compute" = Right Compute
parse ":back" = Right Back
parse ":help" = Right Help
parse ":reset" = Right Reset
parse ":status" = Right Status
parse cmd@(':' : _) = Left $ UnknownCommand cmd
parse word = do
  case isCorrectWord word of
    Left err -> Left $ WordParseError err word
    _ ->  Right $ NewWord word
