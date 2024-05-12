module Unit.Processor.WordProcessorTest(testWordProcessor) where
import Test.Tasty (TestTree, testGroup)
import Data.GameState (WordDiff (WordDiff), Color (Red, Green, Yellow))
import Test.Tasty.HUnit (testCase, (@?=), assertFailure)
import Processor.WordProcessor (calculateDiff, getWordsByWordDiffList, findBestWord, getWordDiffListByWords)
import Control.Exception (ErrorCall (ErrorCall))
import Control.Exception.Base (try)

testWordProcessor :: TestTree
testWordProcessor = testGroup "Word processor test" [
    testCalculateDiff,
    testGetWordsByDiffList,
    testGetWordDiffListByWords,
    testFindBestWord
    ]

testFindBestWord :: TestTree 
testFindBestWord = testGroup "Find best word test" [
    testCase "any option 0 words in dict [...] [] -> Nothing" $ findBestWord ["flask", "brask", "trust"] [] @?= Nothing, 
    testCase "1 option 1 word in dict: [flask] [blown] -> Just flask" $ findBestWord ["flask"] ["blown"] @?= Just "flask",
    testCase "1 option many words in dict: [flask] [blown, fresh] -> Just flask" $ findBestWord ["flask"] ["blown", "fresh"] @?= Just "flask",
    testCase "many options 1 word in dict: [fresh, fleet, seven, fight] [flask] -> Just fleet" $ findBestWord ["fresh", "fleet", "seven", "fight"] ["flask"] @?= Just "fresh",
    testCase "many options many word in dict: [fresh, fleet, seven, fight] [seven, flask, slack, track] -> Just fleet" $ findBestWord ["fresh", "fleet", "seven", "fight"] ["eight", "flask", "slack", "track"] @?= Just "fight",
    testException "[fresh] [slow] -> exception" (try $ return (findBestWord ["fresh"] ["slow"]) :: IO (Either ErrorCall (Maybe String))),
    testException "[fres] [flown] -> exception" (try $ return (findBestWord ["fres"] ["flown"]) :: IO (Either ErrorCall (Maybe String)))
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
    testCase "1 word 0 patterns -> Nothing " $ getWordsByWordDiffList ["trash", "fleet", "treat"] [] @?= Nothing, 
    testCase "1 word 1 pattern: [world] by [(world, ggggg)] -> Just [world]" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "ggggg"] @?= Just ["world"],
    testCase "1 word 1 anti-pattern: [world] by [(world, rrrrr)] -> Just []" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "rrrrr"] @?= Just [],
    testCase "1 word many patterns: [world] by [(world, ggggg), (ldwor, yyyyy)]" $ getWordsByWordDiffList ["world"] [genWordDiff "world" "ggggg", genWordDiff "ldwor" "yyyyy"] @?= Just ["world"],
    testCase "many words 1 pattern: [world, fresh] by [(world, ggggg)] -> Just [world]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "world" "ggggg"] @?= Just ["world"],
    testCase "many words 1 anti-pattern: [world, fresh] by [(xyzap, rrrrr)] -> Just [world, fresh]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "xyzap" "rrrrr"] @?= Just ["world", "fresh"],
    testCase "many words many patterns: [world, fresh] by [(ppppp, rrrrr), (aaaaa, rrrrr)]" $ getWordsByWordDiffList ["world", "fresh"] [genWordDiff "ppppp" "rrrrr", genWordDiff "aaaaa" "rrrrr"] @?= Just ["world", "fresh"],
    testException "[world] by (shrt, yyyy) -> exception" ( try $ return (getWordsByWordDiffList ["world"] [genWordDiff "shrt" "yyyy"]) :: IO (Either ErrorCall (Maybe [String]))),
    testException "[word] by (short, yyyyg) -> exception" ( try $ return (getWordsByWordDiffList ["word"] [genWordDiff "short" "yyyyg"]) :: IO (Either ErrorCall (Maybe [String])))
    ]


testGetWordDiffListByWords :: TestTree
testGetWordDiffListByWords = testGroup "Get word diff list by words test" [
    testCase "'world' by [dread, think, plate] -> [(dread, rrrrr), (dread, rrrrr), (dread, rrrrr)]" $ getWordDiffListByWords "world" ["dread", "think", "plate"] @?= [genWordDiff "dread" "ryrrg", genWordDiff "think" "rrrrr", genWordDiff "plate" "ryrrr"],
    testCase "'world' by [world, ldwor] -> [(world, ggggg), (ldwor, yyyyy)]" $ getWordDiffListByWords "world" ["world", "ldwor"] @?= [genWordDiff "world" "ggggg", genWordDiff "ldwor" "yyyyy"]
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