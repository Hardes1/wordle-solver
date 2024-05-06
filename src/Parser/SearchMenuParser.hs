{-# LANGUAGE InstanceSigs #-}

module Parser.SearchMenuParser (parse, ParseError (..)) where

import Data.Char (toLower)
import Data.SearchMenuCommand (Command (..))
import Debug.Trace (trace)
import Util.WordUtil (isConsistOfLetters, isValidLength)
import Data.GameState (Color(Red, Yellow, Green), WordDiff (WordDiff))

data ErrorType = UnknownCommand | InvalidWordLength | InvalidColorLength | BadWordCharacter | BadColorCharacter

data ParseError = ParseError ErrorType String | InvalidNumberOfWords Int

instance Show ParseError where
  show :: ParseError -> String
  show (ParseError UnknownCommand cmd) = "Unsupported command: '" <> cmd <> "'."
  show (ParseError InvalidWordLength word) = "Word should have length 5. Word '" <> word <> "' has length " <> show (length word) <> "."
  show (ParseError InvalidColorLength color) = "Word should have length 5. Word '" <> color <> "' has length " <> show (length color) <> "."
  show (ParseError BadWordCharacter _) = "Word should only consist of alphabetical symbols."
  show (ParseError BadColorCharacter _) = "Color should consist only of Symbols R|G|B|r|g|b."
  show (InvalidNumberOfWords len) = "Number of words should be two. Actual got " <> show len <> "."

parse :: String -> Either ParseError Command
parse ":search" = Right Search
parse ":quit" = Right Quit
parse ":help" = Right Help
parse ":reset" = Right Reset
parse ":status" = Right Status
parse s@(':' : _) = Left $ ParseError UnknownCommand s
parse input = do
  let wordList = words input
  (word, colorList) <- getWordAndColor wordList
  isCorrectWord word >> isCorrectColorList colorList
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

isCorrectWord :: String -> Either ParseError ()
isCorrectWord word
  | not $ isValidLength word = Left $ ParseError InvalidWordLength word
  | not $ isConsistOfLetters word = Left $ ParseError BadWordCharacter word
  | otherwise = return ()

isCorrectColorList :: String -> Either ParseError ()
isCorrectColorList colorList
  | not $ isValidLength colorList = Left $ ParseError InvalidColorLength colorList
  | not $ isConsistsOfOnlyAllowedColors colorList = Left $ ParseError BadWordCharacter colorList
  | otherwise = return ()

isConsistsOfOnlyAllowedColors :: String -> Bool
isConsistsOfOnlyAllowedColors (chr : rst) =
  ((lchr == 'r') || (lchr == 'y') || (lchr == 'g')) && isConsistsOfOnlyAllowedColors rst
  where
    lchr = toLower chr
isConsistsOfOnlyAllowedColors [] = True
