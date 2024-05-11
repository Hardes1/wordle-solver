{-# LANGUAGE InstanceSigs #-}
module Parser.ComputeMenuCommandParser(parse, ParseError(..), ErrorType(..)) where
import Data.ComputeMenuCommand (Command (..))
import Util.WordUtil (isValidLength, isConsistOfLetters)

data ErrorType = InvalidLength | BadCharacter | UnknownCommand deriving (Eq, Show)

data ParseError = ParseError ErrorType String deriving (Eq)

instance Show ParseError where
    show :: ParseError -> String
    show (ParseError InvalidLength word) = "Word should have length 5. Word '" <> word <> "' has length " <> show (length word) <> "."
    show (ParseError BadCharacter _) = "Word should only consist of alphabetical symbols."
    show (ParseError UnknownCommand cmd) =  "Unsupported command: '" <> cmd <> "'."

parse :: String -> Either ParseError Command
parse ":compute" = Right Compute
parse ":back" = Right Back
parse ":help" = Right Help
parse ":reset" = Right Reset
parse ":status" = Right Status
parse s@(':' : _) = Left $ ParseError UnknownCommand s
parse input = do
  isCorrectWord input
  return $ NewWord input



isCorrectWord :: String -> Either ParseError ()
isCorrectWord word 
    | not $ isValidLength word = Left $ ParseError InvalidLength word
    | not $ isConsistOfLetters word = Left $ ParseError BadCharacter word
    | otherwise = return ()
