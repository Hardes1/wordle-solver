module Parser.GameMenuCommandParser(parse) where
import Data.GameMenuCommand(Command(..))

parse :: String -> Command
parse ":back" = Back
parse ":help" = Help
parse ":guess" = Guess
parse word = Word word
