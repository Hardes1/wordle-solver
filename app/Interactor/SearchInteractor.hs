module Interactor.SearchInteractor(startSearch) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.SearchMenuPrinter(printWelcomeMessage, printHelp)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.SearchMenuCommand(Command(..))
import Data.GameState(WordDiff(..))
import Printer.CommonPrinter(printExit, printParseError)
import Parser.SearchMenuParser(parse)
import Printer.WordDiffPrinter(printWordDiffList)
import WordGenerator (getLaWordList)
import GameProcessor (getWordsByWordDiffList)
import Debug.Trace (trace)

startSearch :: IO ()
startSearch = do
    printWelcomeMessage
    printHelp
    runMaybeT (execStateT loop []) >> printExit

loop :: StateT [WordDiff] (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine
    let parseResult = parse input
    case parseResult of
        Left err -> lift . lift $ printParseError err
        Right cmd -> handleCommand cmd

handleCommand :: Command -> StateT [WordDiff] (MaybeT IO) ()
handleCommand Search = do
    env <- get
    availableWords <- lift . lift $ getLaWordList
    let filteredWordList = getWordsByWordDiffList availableWords env
    trace (show filteredWordList) (return ())
handleCommand (NewWord wordDiff) = modify (wordDiff :)
handleCommand Reset = modify (const [])
handleCommand Help = lift . lift $ printHelp
handleCommand Status = do
    env <- get
    lift . lift $ printWordDiffList $ reverse env

handleCommand Quit = do
    lift . lift $ printExit
    mzero