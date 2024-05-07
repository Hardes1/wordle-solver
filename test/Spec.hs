import Test.Tasty (TestTree, testGroup, defaultMain)
import Unit.Util.WordUtilTest (testWordUtil)
import Unit.Util.ParseUtilTest (testParseUtil)
import Unit.Parser.MainMenuCommandParserTest
import Unit.Parser.GameMenuCommandParserTest (testGameMenuCommandParser)
import Unit.Parser.SearchMenuCommandParser (testSearchMenuCommandParser)

tests :: TestTree
tests = testGroup "Wordle solver tests" [
    testWordUtil,
    testParseUtil,
    testMainMenuCommandParser,
    testGameMenuCommandParser,
    testSearchMenuCommandParser
    ]

main :: IO ()
main = defaultMain tests