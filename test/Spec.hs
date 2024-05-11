import Test.Tasty (TestTree, testGroup, defaultMain)
import Unit.Util.WordUtilTest (testWordUtil)
import Unit.Util.ParseUtilTest (testParseUtil)
import Unit.Parser.MainMenuCommandParserTest
import Unit.Parser.GameMenuCommandParserTest (testGameMenuCommandParser)
import Unit.Parser.SearchMenuCommandParserTest (testSearchMenuCommandParser)
import Unit.Processor.WordProcessorTest (testWordProcessor)

tests :: TestTree
tests = testGroup "Wordle solver tests" [
    testWordUtil,
    testParseUtil,
    testMainMenuCommandParser,
    testGameMenuCommandParser,
    testSearchMenuCommandParser,
    testWordProcessor
    ]

main :: IO ()
main = defaultMain tests