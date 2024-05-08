module Unit.Processor.WordProcessorTest(testWordProcessor) where
import Test.Tasty (TestTree, testGroup)
import Data.GameState (WordDiff (WordDiff), Color (Red, Green, Yellow))
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Processor.WordProcessor (calculateDiff)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Exception.Base (try)


testWordProcessor :: TestTree
testWordProcessor = testGroup "Word processor test" [
    testCalculateDiff
    ]

testCalculateDiff :: TestTree
testCalculateDiff = testGroup "Calculate diff test" [
    testCase "world ~ world -> ggggg" $ calculateDiff "world" "world" @?= genWordDiff "ggggg" "world",
    testCase "abcde ~ fhgijk -> rrrrr" $ calculateDiff "abcde" "fhgij" @?= genWordDiff "rrrrr" "fhgijk",
    testCase "fresh ~ hserf -> yygyy" $ calculateDiff  "fresh" "hserf" @?= genWordDiff "yygyy" "hserf",
    testCase "respects number of letters: aboba ~ bxbyb -> yryrr" $ calculateDiff "aboba" "bxbyb" @?= genWordDiff "yryrr" "bxbyb",
    testCase "mix: fresh ~ forge -> gryry" $ calculateDiff "fresh" "forge" @?= genWordDiff "gryry" "forge",
    testException "word ~ world -> exception" $ calculateDiff "word" "world",
    testException "world ~ word -> exception" $ calculateDiff "world" "word"
    ]

testException :: String -> WordDiff -> TestTree 
testException msg f = testCase msg $ do
    result <- try $ return f :: IO (Either ErrorCall WordDiff)
    case result of
        Left (ErrorCall _) -> assertFailure "Function didn't throw an exception"
        _ -> return ()

genWordDiff :: String -> String -> WordDiff
genWordDiff a b = WordDiff $ zipWith (\x y
  -> (case x of
        'r' -> (Red, y)
        'g' -> (Green, y)
        'y' -> (Yellow, y)
        _ -> error "illegal state")) a b