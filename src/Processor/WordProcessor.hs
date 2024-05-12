module Processor.WordProcessor (calculateDiff, getWordsByWordDiffList, getWordDiffListByWords, findBestWord) where

import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Char (toLower)
import Data.GameState (Color (..), WordDiff (..))
import Data.Map.Strict (adjust, (!?), union)
import Util.WordUtil(areCharsEqual)
import qualified Data.Map.Strict as Map (Map, fromListWith, fromList, unionWith, findWithDefault)

findBestWord :: [String] -> [String] -> Maybe String
findBestWord searchDict inputDict =
  let transformedWords = map (\candidate -> (candidate, getColorCountForDict candidate inputDict)) searchDict
      h = head transformedWords
  in if not $ null inputDict then Just $ fst $ foldl (\x y ->
    if compareWords x y == LT then y else x
    ) h transformedWords
    else Nothing


compareWords :: (String, Map.Map Color Int) -> (String, Map.Map Color Int) -> Ordering
compareWords (_, fiDict) (_, seDict) = compare (scoreMap fiDict) (scoreMap seDict)
    where
      scoreMap m = foldr (\color acc -> acc + (Map.findWithDefault 0 color m * weight color)) 0 [Green, Yellow, Red]
      weight Green  = 50
      weight Yellow = 20
      weight Red    = 2

getColorCountForDict :: String -> [String] -> Map.Map Color Int
getColorCountForDict searchWord inputDict =
  let wordDiffList = getWordDiffListByWords searchWord inputDict
  in foldl (\x y ->
    Map.unionWith (+) x (getColorCountForWordDiff y)
  ) getEmptyCountColorMap wordDiffList


getWordDiffListByWords :: String -> [String] -> [WordDiff]
getWordDiffListByWords base = map (calculateDiff base)

getEmptyCountColorMap :: Map.Map Color Int
getEmptyCountColorMap = Map.fromList [(Red, 0), (Yellow, 0), (Green, 0)]

getColorCountForWordDiff :: WordDiff -> Map.Map Color Int
getColorCountForWordDiff wordDiff =
  Map.fromListWith (+) [(col, 1) | (col, _) <- diffList wordDiff] `union` getEmptyCountColorMap


getWordsByWordDiffList :: [String] -> [WordDiff] -> Maybe [String]
getWordsByWordDiffList wordList wordDiffList = if null wordDiffList then Nothing else Just $ filter (`isWordSatisfyPattern` wordDiffList) wordList

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
      letterCount = Map.fromListWith (+) [(x, 1) | (x, y) <- zip expected actual, not (areCharsEqual x y)]
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
