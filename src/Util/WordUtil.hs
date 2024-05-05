module Util.WordUtil(isCorrectWord) where
import Data.Char (isLower)
import Data.WordError (Error(..))
import Control.Monad (unless)


isValidLength :: String -> Bool
isValidLength word = length word == 5

isAllLowerCase :: String -> Bool
isAllLowerCase = all isLower
isCorrectWord :: String -> Either Error ()
isCorrectWord word = do
    if not $ isValidLength word then Left InvalidLength
    else unless (isAllLowerCase word) $ Left NotInLowerCase