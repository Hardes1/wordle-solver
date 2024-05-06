module GameProcessor(calculateDiff, getWordsByWordDiffList) where
import Data.GameState (WordDiff(..), Color (..))
import qualified Data.Map.Strict as Map(Map, fromListWith)
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Map.Strict (adjust, (!?))
import Data.Char (toLower)
import Debug.Trace (trace)

getWordsByWordDiffList :: [String] -> [WordDiff] -> [String]
getWordsByWordDiffList wordList wordDiffList = filter (`isWordSatisfyPattern` wordDiffList) wordList


areCharsEqual :: Char -> Char -> Bool
areCharsEqual a b = toLower a == toLower b

isWordSatisfyPattern :: String -> [WordDiff] -> Bool
isWordSatisfyPattern word wordDiffList =
    let letterCount = Map.fromListWith (+) [(x, 1) | x <- word]
    in trace ("Word is: " <> word) $ all (\wordDiff -> 
        let wordDiffArr = diffList wordDiff
        in evalState (isWordSatisfyPatternInner word wordDiffArr) letterCount
        ) wordDiffList

isWordSatisfyPatternInner :: String -> [(Color, Char)] -> State (Map.Map Char Int) Bool
isWordSatisfyPatternInner word wordDiffList = do
    afterGY <- isWordSatisfyPatternGY word wordDiffList
    afterR <- isWordSatisfyPatternR word wordDiffList
    return $ afterGY && afterR

isWordSatisfyPatternGY :: String -> [(Color, Char)] -> State (Map.Map Char Int) Bool
isWordSatisfyPatternGY (wordChr:tWord) ((col, patternChr):tcol) = do
    cnt <- get
    case col of 
        Green -> if isContainsAtLeastOneLetter cnt patternChr && areCharsEqual wordChr patternChr then go
        else return False
        Yellow -> if isContainsAtLeastOneLetter cnt patternChr && not (areCharsEqual wordChr patternChr) then go
        else return False
        Red -> isWordSatisfyPatternGY tWord tcol
    where
        go = do
            cnt <- get
            modify $ const (adjust (subtract 1) wordChr cnt)
            isWordSatisfyPatternGY tWord tcol

isWordSatisfyPatternGY [] [] = return True
isWordSatisfyPatternGY _ _ = error "Illegal state exception both word and pattern were different length"

isWordSatisfyPatternR :: String -> [(Color, Char)] -> State (Map.Map Char Int) Bool
isWordSatisfyPatternR (wordChr:tWord) ((col, patternChr):tcol) = do
    cnt <- get
    case col of 
        Red -> if not $ isContainsAtLeastOneLetter cnt patternChr || areCharsEqual wordChr patternChr then isWordSatisfyPatternR tWord tcol
        else return False
        _ -> isWordSatisfyPatternR tWord tcol
isWordSatisfyPatternR [] [] = return True
isWordSatisfyPatternR _ _ = error "Illegal state exception both word and pattern were different length"

calculateDiff :: String -> String -> WordDiff
calculateDiff expected actual =
    let actualTransformed = map toLower actual
        letterCount = Map.fromListWith (+) [(x, 1) | (x, y) <- zip expected actual, x /= y]
    in evalState (calculateDiffInner expected actualTransformed) (letterCount, [])


calculateDiffInner :: String -> String -> State (Map.Map Char Int, [(Color, Char)]) WordDiff
calculateDiffInner (eh:et) (ah:at) = do
    env <- get
    let cnt = fst env
    let lst = snd env
    if ah == eh then modify $ const (cnt, (Green, eh) : lst)
    else if ah /= eh && isContainsAtLeastOneLetter cnt ah then modify $ const (adjust (subtract 1) ah cnt, (Yellow, ah) : lst)
    else modify $ const (cnt, (Red, ah) : lst)
    calculateDiffInner et at
calculateDiffInner "" "" = WordDiff . reverse . snd <$> get
calculateDiffInner _ _ = error "Illegal state exception!, strings were not equal length"

isContainsAtLeastOneLetter :: Map.Map Char Int -> Char -> Bool
isContainsAtLeastOneLetter dict chr = case dict !? chr of
    Nothing -> False
    Just val -> val > 0

