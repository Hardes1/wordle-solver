{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interactor.SearchInteractor(startSearch) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.SearchMenuPrinter(printHelp, printFilteredWords, printNewClue, printAllClues)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.SearchMenuCommand(Command(..))
import Data.GameState(WordDiff(..))
import Printer.CommonPrinter(printWelcomeMessage, printExit, printParseError, printBackExtraInfo, printClearItems)
import Parser.SearchMenuCommandParser(parse)
import Generator.WordGenerator (getLaWordList)
import Processor.WordProcessor (getWordsByWordDiffList)
import Util.ParseUtil (trim)

startSearch :: IO ()
startSearch = do
    printWelcomeMessage "searching word by the set of clues"
    printHelp
    runMaybeT (execStateT loop [])
    printExit

loop :: StateT [WordDiff] (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine
    let parseResult = parse $ trim input
    case parseResult of
        Left err -> lift . lift $ printParseError err
        Right cmd -> handleCommand cmd

handleCommand :: Command -> StateT [WordDiff] (MaybeT IO) ()
handleCommand Search = do
    env <- get
    availableWords <- lift . lift $ getLaWordList
    let filteredWordList = getWordsByWordDiffList availableWords env
    lift . lift $ printFilteredWords (reverse env) (reverse filteredWordList)
handleCommand (NewWord wordDiff) = do
    modify (wordDiff :)
    lift . lift $ printNewClue wordDiff
handleCommand Reset = do 
    modify (const [])
    lift . lift $ printClearItems "clues"
handleCommand Help = lift . lift $ printHelp
handleCommand Status = do
    env <- get
    lift . lift $ printAllClues $ reverse env
handleCommand Back = do
    lift . lift $ printBackExtraInfo "search"
    mzero