
{-# LANGUAGE ScopedTypeVariables #-}

import Data.IORef
import Test.Tasty
import Test.Tasty.HUnit
import System.Exit
import Control.Exception
import Hw0

type Score = IORef (Int, Int)

main :: IO ()
main = do
  sc <- initScore
  defaultMain (tests sc) `catch` (\(e :: ExitCode) -> do
    (n, tot) <- readIORef sc
    putStrLn ("OVERALL SCORE = " ++ show n ++ " / "++ show tot)
    throwIO e)

tests :: Score -> TestTree
tests x = testGroup "Tests"
  [ unit1 x
  , unit2 x
  ]

unit1 :: Score -> TestTree
unit1 sc = testGroup "Unit 1"
  [ mkTest
      sumList
      [1, 2, 3, 4]
      10
      "sample: sumList 1"
  , mkTest
      sumList
      [1, -2, 3, 5]
      7
      "sample: sumList 2"
  , mkTest
      sumList
      [1, 3, 5, 7, 9, 11]
      36
      "sample: sumList 3"
  , mkTest
      digitsOfInt
      3124
      [3, 1, 2, 4]
      "sample: digitsOfInt 1"
  , mkTest
      digitsOfInt
      352663
      [3, 5, 2, 6, 6, 3]
      "sample: digitsOfInt 2"
  , mkTest
      digits
      31243
      [3, 1, 2, 4, 3]
      "sample: digits 1"
  , mkTest
      digits
      (-23422)
      [2, 3, 4, 2, 2]
      "sample: digits 2"
  , mkTest
      additivePersistence
      9876
      2
      "sample: additivePersistence 1"
  , mkTest
      digitalRoot
      9876
      3
      "sample: digitalRoot"
  , mkTest
      listReverse
      [1, 2, 3, 4]
      [4, 3, 2, 1]
      "sample: reverse 1"
  , mkTest
      listReverse
      ["a", "b", "c", "d"]
      ["d", "c", "b", "a"]
      "sample: rev 2"
  , mkTest
      palindrome
      "malayalam"
      True
      "sample: palindrome 1"
  , mkTest
      palindrome
      "myxomatosis"
      False
      "sample: palindrome 2"
  ]
  where
    mkTest :: (Show b, Eq b) => (a -> b) -> a -> b -> String -> TestTree
    mkTest = mkTest' sc

unit2 :: Score -> TestTree
unit2 sc = testGroup "Unit 2" [
  scoreTest (sqSum, [], 0, 1, "sqSum 1"),
  scoreTest (sqSum, [1,2,3,4], 30, 1, "sqSum 2"),
  scoreTest (sqSum, [-1,-2,-3,-4], 30, 1, "sqSum 3"),
  scoreTest (uncurry pipe, ([], 3), 3, 1, "pipe 1"),
  scoreTest (uncurry pipe, ([(\x-> 2*x),(\x -> x + 3)], 3), 12, 1, "pipe 2"),
  scoreTest (uncurry pipe, ([(\x -> x + 3), (\x-> 2*x)], 3), 9, 1, "pipe 3"),

  scoreTest(uncurry sepConcat, (", ",["foo","bar","baz"]), "foo, bar, baz", 1, "sepConcat 1"),
  scoreTest(uncurry sepConcat, ("---",[]), "", 1, "sepConcat 2"),
  scoreTest(uncurry sepConcat, ("",["a","b","c","d","e"]), "abcde", 1, "sepConcat 3"),
  scoreTest(uncurry sepConcat, ("X",["hello"]), "hello", 1, "sepConcat 4"),

  scoreTest(uncurry stringOfList, (intString, [1,2,3,4,5,6]), "[1, 2, 3, 4, 5, 6]",1,"stringOfList 1"),
  scoreTest(uncurry stringOfList, (id, ["foo"]), "[foo]",1,"stringOfList 2"),
  scoreTest(uncurry stringOfList, ((stringOfList intString),[[1,2,3],[4,5],[6],[]]), "[[1, 2, 3], [4, 5], [6], []]",1,"stringOfList 3"),

  scoreTest(uncurry clone, (3,5), [3,3,3,3,3],1,"clone 1"),
  scoreTest(uncurry clone, ("foo",2), ["foo","foo"],1,"clone 2"),

  scoreTest(uncurry padZero, ([9,9],[1,0,0,2]), ([0,0,9,9],[1,0,0,2]),1,"padzero 1"),
  scoreTest(uncurry padZero, ([1,0,0,2],[9,9]), ([1,0,0,2],[0,0,9,9]),1,"padzero 2"),

  scoreTest(removeZero, [0,0,0,1,0,0,2], [1,0,0,2],1,"removeZero 1"),
  scoreTest(removeZero, [9,9], [9,9],1,"removeZero 2"),

  scoreTest(uncurry bigAdd,  ([9,9],[1,0,0,2]), [1,1,0,1],1, "bigAdd 1"),
  scoreTest(uncurry bigAdd,  ([9,9,9,9],[9,9,9]), [1,0,9,9,8],1, "bigAdd 2"),

  scoreTest(uncurry mulByDigit,  (9,[9,9,9,9]), [8,9,9,9,1],1, "mulByDigit 1"),

  scoreTest(uncurry bigMul,  ([9,9,9,9],[9,9,9,9]), [9,9,9,8,0,0,0,1],1, "bigMul 1"),
  scoreTest(uncurry bigMul,  ([9,9,9,9,9],[9,9,9,9,9]), [9,9,9,9,8,0,0,0,0,1],1,"bigMul 2")
  ]
  where
    scoreTest :: (Show b, Eq b) => ((a -> b), a, b, Int, String) -> TestTree
    scoreTest = scoreTest' sc

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
--------------------------------------------------------------------------------
mkTest' :: (Show b, Eq b) => Score -> (a -> b) -> a -> b -> String -> TestTree
--------------------------------------------------------------------------------
mkTest' sc f x r name = scoreTest' sc (f, x, r, 1, name)

--------------------------------------------------------------------------------
scoreTest' :: (Show b, Eq b) => Score -> ((a -> b), a, b, Int, String) -> TestTree
--------------------------------------------------------------------------------
scoreTest' sc (f, x, expR, points, name) =
  testCase name $ do
    updateTotal sc points
    if (f x == expR)
      then updateCurrent sc points
      else assertFailure "Wrong Result"

updateTotal :: Score -> Int -> IO ()
updateTotal sc n = modifyIORef sc (\(x, y) -> (x, y + n))

updateCurrent :: Score -> Int -> IO ()
updateCurrent sc n = modifyIORef sc (\(x, y) -> (x + n, y))

initScore :: IO Score
initScore = newIORef (0, 0)
