module Parser.GameMenuCommandParser(parse) where
import Data.GameMenuCommand(GameMenuCommand(..))

parse :: String -> GameMenuCommand
parse ":back" = Back
parse ":help" = Help
parse word = Word word
