module Unit.Util.ParseUtilTest(testParseUtil) where
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (testCase, (@?=))
import Util.ParseUtil (trim)


testParseUtil :: TestTree
testParseUtil = testGroup "Parse util Test" [
    trimTest
    ]

trimTest :: TestTree 
trimTest = testGroup "Trim test" [
    testCase "'   foo' -> 'foo'" $ trim "   foo" @?= "foo",
    testCase "'foo   ' -> 'foo'" $ trim "foo   " @?= "foo",
    testCase "'   foo   ' -> 'foo'" $ trim "   foo   " @?= "foo",
    testCase "'foo bar' -> 'foo bar'" $ trim "foo bar" @?= "foo bar",
    testCase "'   foo bar' -> 'foo bar'" $ trim "   foo bar" @?= "foo bar",
    testCase "'foo bar    ' -> 'foo bar'" $ trim "foo bar    " @?= "foo bar",
    testCase "'    foo bar    ' -> 'foo bar'" $ trim "    foo bar    " @?= "foo bar"
    ]