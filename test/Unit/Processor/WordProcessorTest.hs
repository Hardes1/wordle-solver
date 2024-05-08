module Unit.Processor.WordProcessorTest(testWordProcessor) where
import Test.Tasty (TestTree, testGroup)
import Data.GameState (WordDiff (WordDiff), Color (Red, Green, Yellow))
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Processor.WordProcessor (calculateDiff, getWordsByWordDiffList)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Exception.Base (try)

testWordProcessor :: TestTree
testWordProcessor = testGroup "Word processor test" [
    testCalculateDiff,
    testGetWordsByDiffList
    ]

testCalculateDiff :: TestTree
testCalculateDiff = testGroup "Calculate diff test" [
    testCase "world ~ world -> ggggg" $ calculateDiff "world" "world" @?= genWordDiff "world" "ggggg",
    testCase "abcde ~ fhgijk -> rrrrr" $ calculateDiff "abcde" "fhgij" @?= genWordDiff "fhgijk" "rrrrr",
    testCase "fresh ~ hserf -> yygyy" $ calculateDiff  "fresh" "hserf" @?= genWordDiff "hserf" "yygyy",
    testCase "respects number of letters: aboba ~ bxbyb -> yryrr" $ calculateDiff "aboba" "bxbyb" @?= genWordDiff "bxbyb" "yryrr",
    testCase "mix: fresh ~ forge -> gryry" $ calculateDiff "fresh" "forge" @?= genWordDiff "forge" "gryry",
    testException "word ~ world -> exception" (try $ return (calculateDiff "word" "world") :: IO (Either ErrorCall WordDiff)),
    testException "world ~ word -> exception" (try $ return (calculateDiff "world" "word") :: IO (Either ErrorCall WordDiff))
    ]

testGetWordsByDiffList :: TestTree
testGetWordsByDiffList = testGroup "Get words by diff list test" [
    testCase "1 word 1 pattern: [world] by [(world, ggggg)] -> [world]" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "ggggg"] @?= ["world"],
    testCase "1 word 1 anti-pattern: [world] by [(world, rrrrr)] -> []" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "rrrrr"] @?= [],
    testCase "1 word many patterns: [world] by [(world, ggggg), (ldwor, yyyyy)]" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "ggggg", genWordDiff "ldwor" "yyyyy"] @?= ["world"],
    testCase "many words 1 pattern: [world, fresh] by [(world, ggggg)] -> [world]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "world" "ggggg"] @?= ["world"],
    testCase "many words 1 anti-pattern: [world, fresh] by [(xyzap, rrrrr)] -> [world, fresh]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "xyzap" "rrrrr"] @?= ["world", "fresh"],
    testCase "many words many patterns: [world, fresh] by [(ppppp, rrrrr), (aaaaa, rrrrr)]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "ppppp" "rrrrr", genWordDiff "aaaaa" "rrrrr"] @?= ["world", "fresh"],
    testException "[world] by (shrt, yyyy) -> exception" ( try $ return (getWordsByWordDiffList ["world"] [genWordDiff "shrt" "yyyy"]) :: IO (Either ErrorCall [String])),
    testException "[word] by (short, yyyyg) -> exception" ( try $ return (getWordsByWordDiffList ["word"] [genWordDiff "short" "yyyyg"]) :: IO (Either ErrorCall [String]))
    ]

testException :: String -> IO (Either ErrorCall a) -> TestTree 
testException msg f = testCase msg $ do
    result <- f
    case result of
        Left (ErrorCall _) -> assertFailure "Function didn't throw an exception"
        _ -> return ()

genWordDiff :: String -> String -> WordDiff
genWordDiff word color = WordDiff $ zipWith (\x y
  -> (case y of
        'r' -> (Red, x)
        'g' -> (Green, x)
        'y' -> (Yellow, x)
        _ -> error "illegal state")) word color