{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interactor.ComputeInteractor(startCompute) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.ComputeMenuPrinter(printHelp, printDictionary)
import Printer.CommonPrinter(printWelcomeMessage, printExit, printBackExtraInfo, printParseError)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.ComputeMenuCommand(Command(..))
import Parser.ComputeMenuCommandParser(parse)
import Util.ParseUtil (trim)
import Util.WordUtil (isKnownWord)

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
    isKnownWordRes <- lift . lift $ isKnownWord word
    case isKnownWordRes of
        Left err -> lift . lift $ putStrLn ("Error: " <> show err)
        _ -> modify (word :)
handleCommand Compute = undefined
handleCommand Reset = modify (const [])
handleCommand Status = do
    env <- get
    lift . lift $ printDictionary $ reverse env
handleCommand Help = lift . lift $ printHelp