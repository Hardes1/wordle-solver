module Util.WordUtil(isCorrectWord, isLastGuessFull, isNumberOfMovesExceeded, isPossibleToMakeMove, maxGuessSteps) where
import Data.WordError (Error(..))
import Data.GameState (WordDiff (WordDiff), Color (..))
import Data.Char (isAlpha)

maxGuessSteps :: Int
maxGuessSteps = 6

isValidLength :: String -> Bool
isValidLength word = length word == 5

isConsistOfLetters :: String -> Bool
isConsistOfLetters = all isAlpha

isCorrectWord :: String -> Either Error ()
isCorrectWord word = do
    if not $ isValidLength word then Left (InvalidLength (length word))
    else if not $ isConsistOfLetters word then Left BadCharacter
    else Right ()

isLastGuessFull :: [WordDiff] -> Bool
isLastGuessFull (WordDiff arr:_) = all ((== Green) . fst) arr
isLastGuessFull _ = False

isNumberOfMovesExceeded :: [WordDiff] -> Bool
isNumberOfMovesExceeded arr = length arr > maxGuessSteps

isPossibleToMakeMove :: [WordDiff] -> Bool
isPossibleToMakeMove arr = length arr < maxGuessSteps