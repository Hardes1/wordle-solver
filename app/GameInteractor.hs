module GameInteractor(startGame) where
import Control.Monad (MonadPlus(mzero), forever)
import Control.Monad.Trans.State (StateT, get)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.GameState(GameState(..))
import Data.Char(isUpper)
import Parser.GameMenuCommandParser(parse)
import Data.GameMenuCommand (GameMenuCommand(..))
import Printer.GameMenuPrinter (printHelp)

startGame :: IO ()
startGame = do
    putStrLn "Welcome to the wordle game!"
    printHelp
    runMaybeT $ evalStateT loop (GameState [])
    putStrLn "You finished the game. Leaving to the main menu..."

loop :: StateT GameState (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine -- add logic to finish game
    handleCommand $ parse input

handleCommand :: GameMenuCommand -> StateT GameState (MaybeT IO) ()
handleCommand Back = mzero
handleCommand Help = lift . lift $ printHelp
handleCommand (Word word) = do
    if not $ isValidLength word then lift . lift $ putStrLn "The word must be exactly 5 letters long!"
    else if not $ isAllUpperCase word then lift . lift $ putStrLn "The word must consist only from upper-case letters!"
    else do
        env <- get

        return ()

isValidLength :: String -> Bool
isValidLength word = length word == 5

isAllUpperCase :: String -> Bool
isAllUpperCase = all isUpper 