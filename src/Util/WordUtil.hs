module Util.WordUtil(isCorrectWord, isLastGuessFull, isNumberOfMovesExceeded, isPossibleToMakeMove) where
import Data.Char (isLower)
import Data.WordError (Error(..))
import Control.Monad (unless)
import Data.GameState (WordDiff (WordDiff), Color (..))


isValidLength :: String -> Bool
isValidLength word = length word == 5

isAllLowerCase :: String -> Bool
isAllLowerCase = all isLower

isCorrectWord :: String -> Either Error ()
isCorrectWord word = do
    if not $ isValidLength word then Left InvalidLength
    else unless (isAllLowerCase word) $ Left NotInLowerCase

isLastGuessFull :: [WordDiff] -> Bool
isLastGuessFull (WordDiff arr:_) = all ((== Green) . fst) arr
isLastGuessFull _ = False

isNumberOfMovesExceeded :: [WordDiff] -> Bool
isNumberOfMovesExceeded arr = length arr > 5

isPossibleToMakeMove :: [WordDiff] -> Bool
isPossibleToMakeMove arr = length arr < 5