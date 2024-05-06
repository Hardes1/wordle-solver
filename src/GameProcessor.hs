module GameProcessor(calculateDiff) where
import Data.GameState (WordDiff(..), Color (..))
import qualified Data.Map.Strict as Map(Map, fromListWith)
import Control.Monad.Trans.State (State, evalState, get, modify)
import Data.Map.Strict (adjust, (!?))
import Data.Char (toLower)

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

