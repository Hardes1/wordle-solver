{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interactor.ComputeInteractor(startCompute) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.ComputeMenuPrinter(printHelp, printDictionary, printComputeResult, printNewWord)
import Printer.CommonPrinter(printWelcomeMessage, printExit, printBackExtraInfo, printParseError, printClearItems, printNoItems)
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
handleCommand (NewWord word) = do 
    modify (word :)
    lift . lift $ printNewWord word
handleCommand Compute = do
    env <- get
    laDict <- lift . lift $ getLaWordList
    taDict <- lift . lift $ getTaWordList
    let allPossibleWords = laDict ++ taDict
        bestWord = findBestWord allPossibleWords env
    case bestWord of
        Just val -> lift . lift $ printComputeResult val (reverse $ getWordDiffListByWords val env)
        _ -> lift . lift $ printNoItems "words"
    
handleCommand Reset = do 
    modify (const [])
    lift . lift $ printClearItems "words"
handleCommand Status = do
    env <- get
    lift . lift $ printDictionary $ reverse env
handleCommand Help = lift . lift $ printHelp