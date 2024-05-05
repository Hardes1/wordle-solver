{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use when" #-}
module Interactor.GameInteractor(startGame) where
import Control.Monad (MonadPlus(mzero), forever)
import Control.Monad.Trans.State (StateT, get, modify)
import Control.Monad.Trans.Maybe (MaybeT (runMaybeT))
import Control.Monad.Trans.State.Lazy (execStateT)
import Control.Monad.Trans.Class (MonadTrans(lift))
import Data.GameState(GameState(..), WordDiff(..), GameStatus(..), IncorrectStatus(..))
import Util.WordUtil(isCorrectWord, isLastGuessFull, isNumberOfMovesExceeded, isPossibleToMakeMove)
import Data.Either
import Parser.GameMenuCommandParser(parse)
import Data.GameMenuCommand (Command(..))
import Printer.GameMenuPrinter (printHelp, printWelcomeMessage, printWordDiff, printGameResult, printHUD)
import GameProcessor (calculateDiff)
import Debug.Trace (trace)

genWord :: String
genWord = "pidor"

startGame :: IO ()
startGame = do
    printWelcomeMessage
    printHelp
    let word = genWord
    env <- execStateT (runMaybeT loop) (CorrectState InProgress [] word)
    printGameResult env

loop :: MaybeT (StateT GameState IO) ()
loop = forever $ do
    determineGameState
    env <- lift get
    lift . lift $ printHUD env
    input <- lift . lift $ getLine -- add logic to finish game
    handleCommand $ parse input

determineGameState :: MaybeT (StateT GameState IO) ()
determineGameState = do
    env <- lift get
    let (wordDiffList, guessWord) = extractDataFromEnv env
    if isLastGuessFull wordDiffList && not (isNumberOfMovesExceeded wordDiffList) then do
        lift $ modify (const $ CorrectState Win wordDiffList guessWord)
        mzero
    else if not (isPossibleToMakeMove wordDiffList) then do
        lift $ modify (const $ CorrectState Lose wordDiffList guessWord)
        mzero
    else return ()


handleCommand :: Command -> MaybeT (StateT GameState IO) ()
handleCommand Back = do
    lift $ modify (\_ -> IncorrectState Cancelled)
    mzero
handleCommand Help = lift . lift $ printHelp
handleCommand (Word input) = do
    env <- lift get
    let (wordDiffList, guessWord) = extractDataFromEnv env
    case isCorrectWord input of
        Left err -> lift . lift $ putStrLn ("Error: " <> show err)
        Right _ -> do
            let diff = calculateDiff guessWord input
            lift . lift $ printWordDiff diff
            lift $ modify (const $ CorrectState InProgress (diff:wordDiffList) guessWord)
            return ()


extractDataFromEnv :: GameState -> ([WordDiff], String)
extractDataFromEnv state = case state of 
    CorrectState InProgress wordDiffList guessWord -> (wordDiffList, guessWord)
    _ -> error "Illegal state exception. Trying to extract from inconsistent data"