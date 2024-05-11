{-# LANGUAGE InstanceSigs #-}
module Parser.SearchMenuCommandParser (parse, ParseError (..)) where

import Data.Char (toLower)
import Data.SearchMenuCommand (Command (..))
import Data.GameState (Color(Red, Yellow, Green), WordDiff (WordDiff))
import qualified Data.WordError as WordError(ParseError(..))
import qualified Data.ColorError as ColorError(ParseError(..))
import Util.ParseUtil (isCorrectWord, isCorrectColorList)

data ParseError = WordParseError WordError.ParseError String | ColorParseError ColorError.ParseError String | UnknownCommand String | InvalidNumberOfWords Int deriving Eq

instance Show ParseError where
  show :: ParseError -> String
  show (UnknownCommand cmd) = "Unsupported command: '" <> cmd <> "'."
  show (InvalidNumberOfWords len) = "Number of words should be two. Actual got " <> show len <> "."
  show (WordParseError err word) = "Parse error: " <> "Word '" <> word <> "'. "
  show (ColorParseError err word) = "Parse error: " <> "Color '" <> word <> "'. "

parse :: String -> Either ParseError Command
parse ":search" = Right Search
parse ":back" = Right Back
parse ":help" = Right Help
parse ":reset" = Right Reset
parse ":status" = Right Status
parse s@(':' : _) = Left $ UnknownCommand s
parse input = do
  let wordList = words input
  (word, colorList) <- getWordAndColor wordList
  wrapWordError isCorrectWord word >> wrapParseError isCorrectColorList colorList
  return $ NewWord $ WordDiff (combineWords word colorList)
  where
    combineWords = zipWith (\letter color
      -> (case toLower color of
            'r' -> (Red, letter)
            'y' -> (Yellow, letter)
            'g' -> (Green, letter)
            _ -> error "Illegal state exception. Unexpected color"))


getWordAndColor :: [String] -> Either ParseError (String, String)
getWordAndColor [a, b] = Right (a, b)
getWordAndColor arr = Left $ InvalidNumberOfWords (length arr)

wrapWordError :: (String -> Either WordError.ParseError ()) -> String -> Either ParseError ()
wrapWordError f word = case f word of
  Left err -> Left $ WordParseError err word
  _ -> Right ()

wrapParseError :: (String -> Either ColorError.ParseError ()) -> String -> Either ParseError ()
wrapParseError f word = case f word of
  Left err -> Left $ ColorParseError err word
  _ -> Right ()