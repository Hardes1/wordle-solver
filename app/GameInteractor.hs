module GameInteractor(startGame) where
import Control.Monad (MonadPlus(mzero), forever)
import Control.Monad.Trans.State (StateT, get)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (evalStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.GameState(GameState(..))
import Util.WordUtil(isCorrectWord)
import Data.Either
import Parser.GameMenuCommandParser(parse)
import Data.GameMenuCommand (Command(..))
import Printer.GameMenuPrinter (printHelp, printWelcomeMessage, printWordDiff)
import GameProcessor (calculateDiff)

startGame :: IO ()
startGame = do
    printWelcomeMessage
    printHelp
    runMaybeT $ evalStateT loop (GameState [])
    putStrLn "You finished the game. Leaving to the main menu..."

loop :: StateT GameState (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine -- add logic to finish game
    handleCommand $ parse input

handleCommand :: Command -> StateT GameState (MaybeT IO) ()
handleCommand Back = mzero
handleCommand Help = lift . lift $ printHelp
handleCommand (Word word) = case isCorrectWord word of
        Left err -> lift . lift $ putStrLn ("Error: " <> show err)
        Right _ -> do 
            let diff = calculateDiff "pidor" word
            lift . lift $ printWordDiff diff
            return ()
