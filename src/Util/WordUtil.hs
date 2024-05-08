module Util.WordUtil(isKnownWord, isLastGuessFull, isNumberOfMovesExceeded, isPossibleToMakeMove, maxGuessSteps, isConsistOfLetters, isValidLength) where
import Data.WordError(Error(..))
import Data.GameState (WordDiff (WordDiff), Color (..))
import Generator.WordGenerator (getLaWordList, getTaWordList)
import Data.Char (isAlpha, toLower)

maxGuessSteps :: Int
maxGuessSteps = 6

isKnownWord :: String -> IO (Either Error ())
isKnownWord word = do
    let lowerWord = map toLower word
    taList <- getLaWordList
    laList <- getTaWordList
    return (if elem lowerWord taList || elem lowerWord laList then Right () else  Left UnknownWord)

isLastGuessFull :: [WordDiff] -> Bool
isLastGuessFull (WordDiff arr:_) = all ((== Green) . fst) arr
isLastGuessFull _ = False

isNumberOfMovesExceeded :: [WordDiff] -> Bool
isNumberOfMovesExceeded arr = length arr > maxGuessSteps

isPossibleToMakeMove :: [WordDiff] -> Bool
isPossibleToMakeMove arr = length arr < maxGuessSteps

isConsistOfLetters :: String -> Bool
isConsistOfLetters = all isAlpha

isValidLength :: String -> Bool
isValidLength word = length word == 5