import Test.Tasty (TestTree, testGroup, defaultMain)
import Util.WordUtilTest (wordUtilTest)
import Util.ParseUtilTest (parseUtilTest)

tests :: TestTree
tests = testGroup "Wordle solver tests" [
    wordUtilTest,
    parseUtilTest
    ]

main :: IO ()
main = defaultMain tests