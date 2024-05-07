import Test.Tasty (TestTree, testGroup, defaultMain)
import Unit.Util.WordUtilTest (wordUtilTest)
import Unit.Util.ParseUtilTest (parseUtilTest)

tests :: TestTree
tests = testGroup "Wordle solver tests" [
    wordUtilTest,
    parseUtilTest
    ]

main :: IO ()
main = defaultMain tests