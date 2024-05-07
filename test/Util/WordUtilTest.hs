module Util.WordUtilTest (wordUtilTest) where

import Data.GameState (Color (Green, Red, Yellow), WordDiff (WordDiff))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util.WordUtil (isConsistOfLetters, isLastGuessFull, isNumberOfMovesExceeded, isPossibleToMakeMove, isValidLength, maxGuessSteps)

wordUtilTest :: TestTree
wordUtilTest =
  testGroup
    "Word util test"
    [ testMaxGuessSteps,
      testConsistOfLetters,
      testValidLength,
      testLastGuess,
      testNumberOfMovesExceeded,
      testPossibleToMakeMove
    ]

testMaxGuessSteps :: TestTree
testMaxGuessSteps = testGroup "Max guess step" [
    testCase "MaxGuessSteps is equal to six" $ maxGuessSteps @?= 6
    ]

testConsistOfLetters :: TestTree
testConsistOfLetters =
  testGroup
    "Consist letters test"
    [ testCase "fresh -> True" $ isConsistOfLetters "fresh" @?= True,
      testCase "FRESH -> True" $ isConsistOfLetters "FRESh" @?= True,
      testCase ":cmd -> False" $ isConsistOfLetters ":cmd" @?= False
    ]

testValidLength :: TestTree
testValidLength =
  testGroup
    "Valid length test"
    [ testCase "fresh -> True" $ isValidLength "fresh" @?= True,
      testCase "FRESH -> True" $ isValidLength "FRESH" @?= True,
      testCase "[] -> False" $ isValidLength [] @?= False,
      testCase "loooong -> False" $ isValidLength "loooong" @?= False,
      testCase "shot -> False" $ isValidLength "shot" @?= False,
      testCase "a -> False" $ isValidLength "a" @?= False
    ]

testLastGuess :: TestTree
testLastGuess =
  testGroup
    "Last guess test"
    [ testCase "GGGGG -> True" $ isLastGuessFull [genWordDiff "GGGGG"] @?= True,
      testCase "GGGGR -> False" $ isLastGuessFull [genWordDiff "GGGGR"] @?= False,
      testCase "GGGGY -> False" $ isLastGuessFull [genWordDiff "GGGGY"] @?= False,
      testCase "[] -> True" $ isLastGuessFull [genWordDiff ""] @?= True
    ]

testNumberOfMovesExceeded :: TestTree
testNumberOfMovesExceeded =
  testGroup
    "Number of moves exceeded"
    [ testCase "0 -> False" $ isNumberOfMovesExceeded (replicate 0 (WordDiff [])) @?= False,
      testCase "1 -> False" $ isNumberOfMovesExceeded (replicate 1 (WordDiff [])) @?= False,
      testCase "2 -> False" $ isNumberOfMovesExceeded (replicate 2 (WordDiff [])) @?= False,
      testCase "3 -> False" $ isNumberOfMovesExceeded (replicate 3 (WordDiff [])) @?= False,
      testCase "4 -> False" $ isNumberOfMovesExceeded (replicate 4 (WordDiff [])) @?= False,
      testCase "5 -> False" $ isNumberOfMovesExceeded (replicate 5 (WordDiff [])) @?= False,
      testCase "6 -> False" $ isNumberOfMovesExceeded (replicate 6 (WordDiff [])) @?= False,
      testCase "7 -> False" $ isNumberOfMovesExceeded (replicate 7 (WordDiff [])) @?= True
    ]

testPossibleToMakeMove :: TestTree
testPossibleToMakeMove =
  testGroup
    "Possible to make move"
    [ testCase "0 -> True" $ isPossibleToMakeMove (replicate 0 (WordDiff [])) @?= True,
      testCase "1 -> True" $ isPossibleToMakeMove (replicate 1 (WordDiff [])) @?= True,
      testCase "2 -> True" $ isPossibleToMakeMove (replicate 2 (WordDiff [])) @?= True,
      testCase "3 -> True" $ isPossibleToMakeMove (replicate 3 (WordDiff [])) @?= True,
      testCase "4 -> True" $ isPossibleToMakeMove (replicate 4 (WordDiff [])) @?= True,
      testCase "5 -> True" $ isPossibleToMakeMove (replicate 5 (WordDiff [])) @?= True,
      testCase "6 -> False" $ isPossibleToMakeMove (replicate 6 (WordDiff [])) @?= False
    ]

genWordDiff :: String -> WordDiff
genWordDiff s = WordDiff $ map toColorPair s
  where
    toColorPair 'R' = (Red, 'a')
    toColorPair 'G' = (Green, 'a')
    toColorPair 'Y' = (Yellow, 'a')
    toColorPair _ = error "Illegal state"