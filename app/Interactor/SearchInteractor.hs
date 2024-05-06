module Interactor.SearchInteractor(startSearch) where
import Control.Monad (MonadPlus(mzero), forever)
import Printer.SearchMenuPrinter(printWelcomeMessage, printHelp)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.GameState(WordDiff(..))
import Printer.CommonPrinter(printExit)

startSearch :: IO ()
startSearch = do
    printWelcomeMessage
    printHelp
    runMaybeT (execStateT loop []) >> printExit

loop :: StateT [WordDiff] (MaybeT IO) ()
loop = forever $ do
    input <- lift . lift $ getLine
    lift . lift $ putStrLn input
    