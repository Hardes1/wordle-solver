module Processor.WordProcessor (calculateDiff, getWordsByWordDiffList) where

import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Char (toLower)
import Data.GameState (Color (..), WordDiff (..))
import Data.Map.Strict (adjust, (!?))
import qualified Data.Map.Strict as Map (Map, fromListWith)

getWordsByWordDiffList :: [String] -> [WordDiff] -> [String]
getWordsByWordDiffList wordList wordDiffList = filter (`isWordSatisfyPattern` wordDiffList) wordList

areCharsEqual :: Char -> Char -> Bool
areCharsEqual a b = toLower a == toLower b

isWordSatisfyPattern :: String -> [WordDiff] -> Bool
isWordSatisfyPattern word wordDiffList =
  let letterCount = Map.fromListWith (+) [(x, 1) | x <- word]
   in all
        ( \wordDiff ->
            let wordDiffArr = diffList wordDiff
             in evalState (isWordSatisfyPatternInner word wordDiffArr) letterCount
        )
        wordDiffList

isWordSatisfyPatternInner :: String -> [(Color, Char)] -> State (Map.Map Char Int) Bool
isWordSatisfyPatternInner word wordDiffList = do
  let isWordSatisfyColorPartially = isWordSatisfyColor word wordDiffList
  afterG <- isWordSatisfyColorPartially Green (&&) (adjust (subtract 1))
  afterY <- isWordSatisfyColorPartially Yellow (\x y -> x && not y) (adjust (subtract 1))
  afterR <- isWordSatisfyColorPartially Red (\x y -> not (x || y)) (\_ y -> y)
  return $ afterG && afterY && afterR

isWordSatisfyColor :: String -> [(Color, Char)] -> Color -> (Bool -> Bool -> Bool) -> (Char -> Map.Map Char Int -> Map.Map Char Int) -> State (Map.Map Char Int) Bool
isWordSatisfyColor (wordChr : tWord) ((col, patternChr) : tcol) currentCol condPred trans = do
  cnt <- get
  let condOne = isContainsAtLeastOneLetter cnt patternChr
      condTwo = areCharsEqual wordChr patternChr
  if col == currentCol && condPred condOne condTwo then go
  else if col == currentCol then return False
  else isWordSatisfyColor tWord tcol currentCol condPred trans
  where
    go = do
      modify (trans patternChr)
      isWordSatisfyColor tWord tcol currentCol condPred trans
isWordSatisfyColor [] [] _ _ _ = return True
isWordSatisfyColor _ _ _ _ _ = error "Illegal state exception both word and pattern were different length"

calculateDiff :: String -> String -> WordDiff
calculateDiff expected actual =
  let actualTransformed = map toLower actual
      letterCount = Map.fromListWith (+) [(x, 1) | (x, y) <- zip expected actual, x /= y]
   in evalState (calculateDiffInner expected actualTransformed) (letterCount, [])

calculateDiffInner :: String -> String -> State (Map.Map Char Int, [(Color, Char)]) WordDiff
calculateDiffInner (eh : et) (ah : at) = do
  env <- get
  let cnt = fst env
  let lst = snd env
  if ah == eh
    then modify $ const (cnt, (Green, eh) : lst)
    else
      if ah /= eh && isContainsAtLeastOneLetter cnt ah
        then modify $ const (adjust (subtract 1) ah cnt, (Yellow, ah) : lst)
        else modify $ const (cnt, (Red, ah) : lst)
  calculateDiffInner et at
calculateDiffInner "" "" = WordDiff . reverse . snd <$> get
calculateDiffInner _ _ = error "Illegal state exception!, strings were not equal length"

isContainsAtLeastOneLetter :: Map.Map Char Int -> Char -> Bool
isContainsAtLeastOneLetter dict chr = case dict !? chr of
  Nothing -> False
  Just val -> val > 0
