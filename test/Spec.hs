import Test.Tasty (TestTree, testGroup, defaultMain)
import Unit.Util.WordUtilTest (testWordUtil)
import Unit.Util.ParseUtilTest (testParseUtil)
import Unit.Parser.MainMenuCommandParserTest
import Unit.Parser.GameMenuCommandParserTest (testGameMenuCommandParser)
import Unit.Parser.SearchMenuCommandParserTest (testSearchMenuCommandParser)
import Unit.Processor.WordProcessorTest (testWordProcessor)
import Unit.Parser.ComputeMenuCommandParserText (testComputeMenuCommandParser)

tests :: TestTree
tests = testGroup "Wordle solver tests" [
    testWordUtil,
    testParseUtil,
    testMainMenuCommandParser,
    testGameMenuCommandParser,
    testSearchMenuCommandParser,
    testComputeMenuCommandParser,
    testWordProcessor
    ]

main :: IO ()
main = defaultMain tests