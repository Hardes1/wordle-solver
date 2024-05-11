{-# OPTIONS_GHC -Wno-unused-do-bind #-}
module Interactor.ComputeInteractor(startCompute) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.ComputeMenuPrinter(printHelp)
import Printer.CommonPrinter(printWelcomeMessage, printExit, printBackExtraInfo)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.ComputeMenuCommand(Command(..))
import Printer.CommonPrinter(printExit, printParseError)
import Parser.ComputeMenuCommandParser(parse)
import Util.ParseUtil (trim)

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
handleCommand _ = undefined
