module Util.WordUtil(isCorrectWord) where
import Data.Char (isUpper)
import Data.WordError (Error(..))
import Control.Monad (unless)


isValidLength :: String -> Bool
isValidLength word = length word == 5

isAllUpperCase :: String -> Bool
isAllUpperCase = all isUpper

isCorrectWord :: String -> Either Error ()
isCorrectWord word = do
    if not $ isValidLength word then Left InvalidLength
    else unless (isAllUpperCase word) $ Left NotInUpperCase