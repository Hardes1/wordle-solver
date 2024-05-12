{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interactor.ComputeInteractor(startCompute) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.ComputeMenuPrinter(printHelp, printDictionary, printComputeResult)
import Printer.CommonPrinter(printWelcomeMessage, printExit, printBackExtraInfo, printParseError)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.ComputeMenuCommand(Command(..))
import Parser.ComputeMenuCommandParser(parse)
import Util.ParseUtil (trim)
import Generator.WordGenerator (getTaWordList, getLaWordList)
import Processor.WordProcessor (findBestWord, getWordDiffListByWords)

startCompute :: IO ()
startCompute = do
    printWelcomeMessage "compute best word by set of words"
    printHelp
    runMaybeT (execStateT loop [])
    printExit

loop :: StateT [String] (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine
    let parseResult = parse $ trim input
    case parseResult of
        Left err -> lift . lift $ printParseError err
        Right cmd -> handleCommand cmd

handleCommand :: Command -> StateT [String] (MaybeT IO) ()
handleCommand Back = do
    lift . lift $ printBackExtraInfo "compute"
    mzero
handleCommand (NewWord word) = modify (word :)
handleCommand Compute = do
    env <- get
    laDict <- lift . lift $ getLaWordList
    taDict <- lift . lift $ getTaWordList
    let allPossibleWords = laDict ++ taDict
        bestWord = findBestWord allPossibleWords env
        wordDiffList = getWordDiffListByWords bestWord env
    lift . lift $ printComputeResult bestWord (reverse wordDiffList)
handleCommand Reset = modify (const [])
handleCommand Status = do
    env <- get
    lift . lift $ printDictionary $ reverse env
handleCommand Help = lift . lift $ printHelp