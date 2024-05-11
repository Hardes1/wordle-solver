module Util.ParseUtil(trim, isCorrectWord, isCorrectColorList) where
import Data.List (dropWhileEnd)
import Data.Char (isSpace)
import qualified Data.WordError as WordError(ParseError(..))
import qualified Data.ColorError as ColorError(ParseError(..))
import Util.WordUtil (isValidLength, isConsistOfLetters)
import GHC.Unicode (toLower)
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace


isCorrectWord :: String -> Either WordError.ParseError ()
isCorrectWord word
  | not $ isValidLength word = Left WordError.InvalidWordLength
  | not $ isConsistOfLetters word = Left WordError.BadWordCharacter
  | otherwise = return ()


isCorrectColorList :: String -> Either ColorError.ParseError ()
isCorrectColorList colorList
  | not $ isValidLength colorList = Left ColorError.InvalidColorLength
  | not $ isConsistsOfOnlyAllowedColors colorList = Left ColorError.BadColorCharacter
  | otherwise = return ()


isConsistsOfOnlyAllowedColors :: String -> Bool
isConsistsOfOnlyAllowedColors (chr : rst) =
  ((lchr == 'r') || (lchr == 'y') || (lchr == 'g')) && isConsistsOfOnlyAllowedColors rst
  where
    lchr = toLower chr
isConsistsOfOnlyAllowedColors [] = True