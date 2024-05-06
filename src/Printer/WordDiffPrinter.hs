module Printer.WordDiffPrinter(printWordDiff, printWordDiffList) where
import Data.Foldable (traverse_)
import Data.GameState (WordDiff(..), Color(..))
import Data.Char (toUpper)

printWordDiff :: WordDiff -> IO ()
printWordDiff (WordDiff arr) = do
     traverse_ (\(color, chr) -> case color of
          Green -> putStr $ "\ESC[32m" <> [toUpper chr] <> "\ESC[0m"
          Yellow -> putStr $ "\ESC[33m" <> [toUpper chr] <> "\ESC[0m"
          Red -> putStr $ "\ESC[31m" <> [toUpper chr] <> "\ESC[0m") arr
     putStrLn []

printWordDiffList :: [WordDiff] -> IO ()
printWordDiffList = traverse_ printWordDiff 
