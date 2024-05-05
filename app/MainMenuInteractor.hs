module MainMenuInteractor(run) where

import ConsolePrintUtil(printWelcomeMessage, printHelp, printExit)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.Class (lift)
import Control.Monad (forever, MonadPlus (mzero))
import Data.Command (MainMenuCommand (..))
import Parser.CommandParser(parseMainMenuCommand)

run :: IO ()
run = do
    printWelcomeMessage
    printHelp
    runMaybeT loop >> printExit

loop :: MaybeT IO ()
loop = forever $ do
    input <- lift getLine
    case parseMainMenuCommand input of 
        Nothing -> lift $ putStrLn ("Unknown command: '" <> input <> "'")
        Just cmd -> handleCommand cmd

handleCommand :: MainMenuCommand -> MaybeT IO ()
handleCommand Game = undefined
handleCommand Search = undefined
handleCommand Compute = undefined
handleCommand Help = lift printHelp
handleCommand Quit = mzero


