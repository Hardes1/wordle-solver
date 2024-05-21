module Unit.Parser.MainMenuCommandParserTest(testMainMenuCommandParser) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Parser.MainMenuCommandParser (parse)
import Data.MainMenuCommand (Command(..))

testMainMenuCommandParser :: TestTree
testMainMenuCommandParser = testGroup "Main menu command parser Test" [
    testCase ":game -> Just Game" $ parse ":game" @?= Just Game,
    testCase ":help -> Just Game" $ parse ":help" @?= Just Help,
    testCase ":quit -> Just Game" $ parse ":quit" @?= Just Quit,
    testCase ":search -> Just Game" $ parse ":search" @?= Just Search,
    testCase ":compute -> Just Game" $ parse ":compute" @?= Just Compute,
    testCase "smth -> Nothing" $ parse "smth" @?= Nothing
   ]

