module Interactor.MainInteractor(run) where

import Printer.MainMenuPrinter(printWelcomeMessage, printHelp, printExit)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (forever, MonadPlus (mzero))
import Data.MainMenuCommand (Command (..))
import Interactor.GameInteractor(startGame)
import Interactor.SearchInteractor(startSearch)
import Parser.MainMenuCommandParser(parse)
import Util.ParseUtil (trim)

run :: IO ()
run = do
    printWelcomeMessage
    printHelp
    runMaybeT loop >> printExit

loop :: MaybeT IO ()
loop = forever $ do
    input <- lift getLine
    case parse $ trim input of 
        Nothing -> lift $ putStrLn ("Unknown command: '" <> input <> "'")
        Just cmd -> handleCommand cmd

handleCommand :: Command -> MaybeT IO ()
handleCommand Game = lift startGame
handleCommand Search = lift startSearch
handleCommand Compute = undefined
handleCommand Help = lift printHelp
handleCommand Quit = mzero


