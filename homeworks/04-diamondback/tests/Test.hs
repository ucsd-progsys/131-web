{-# LANGUAGE ScopedTypeVariables #-}

import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Control.Exception
import System.FilePath                  ((</>), (<.>))
import System.IO                        (withFile)
import System.Exit
import Data.List                        (isInfixOf)
import Data.Char (toLower)
import Language.Diamondback.Utils
import Language.Diamondback.Types hiding (Program, Result)
import Language.Diamondback.Parser
import Language.Diamondback.Normalizer
import Debug.Trace (trace)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ testGroup "Normalizer"      anfTests
  , testGroup "Adder"           adderTests
  , testGroup "Boa"             boaTests
  , testGroup "Cobra"           cobraTests
  , testGroup "Diamond"         diamondTests
  , testGroup "Dynamic-Errors"  dynamicTests
  , testGroup "Static-Errors"   staticTests
  ]

anfTests =
  [ anfTest "prim1"
      "add1(add1(add1(add1(x))))"
      "(let anf0 = add1(x), anf1 = add1(anf0), anf2 = add1(anf1) in add1(anf2))"

  , anfTest "prim2"
      "((2 + 3) * (12 - 4)) * (7 + 8)"
      "(let anf0 = 2 + 3, anf1 = 12 - 4, anf2 = anf0 * anf1, anf3 = 7 + 8 in anf2 * anf3)"

  , anfTest "let-1"
      "(let x = 10 in x + 5) + (let y = 20 in y - 5)"
      "(let anf0 = (let x = 10 in x + 5), anf1 = (let y = 20 in y - 5) in anf0 + anf1)"

  , anfTest "if-1"
      "(if x: y + 1 else: z + 1) + 12"
      "(let anf0 = (if x: y + 1 else: z + 1) in anf0 + 12)"
  ]

adderTests =
  [ mkTest "forty_one"  (Code "41")               (Right "41")
  , mkTest "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest "five"        File                     (Right "5")
  , mkTest "adds"        File                     (Right "8")
  , mkTest "subs"        File                     (Right "8")
  , mkTest "lets"        File                     (Right "14")
  , mkTest "expr0"       File                     (Right "600")
  ]

boaTests =
  [ mkTest "expr1"       File      (Right "30")
  , mkTest "expr2"       File      (Right "20")
  , mkTest "expr3"       File      (Right "20")
  , mkTest "expr4"       File      (Right "-8")
  , mkTest "exp00"       File      (Right "65536")
  ]

cobraTests =
  [ mkTest "neg00"       File      (rLines ["-3"])
  , mkTest "neg01"       File      (rLines ["-2"])
  , mkTest "print0"      File      (rLines ["12", "12"])
  , mkTest "print1"      File      (rLines ["true", "true"])
  , mkTest "print2"      File      (rLines ["false", "false"])
  , mkTest "print3"      File      (rLines ["2", "4", "4"])
  , mkTest "bool0"       File      (rLines ["false", "true"])
  , mkTest "bool1"       File      (rLines ["10", "true"])
  , mkTest "bool2"       File      (rLines ["6", "false", "false"])
  , mkTest "bool3"       File      (rLines ["44"])
  , mkTest "bool4"       File      (rLines ["100", "true", "200"])
  , mkTest "dyn0"        File      (rLines ["120"])
  , mkTest "dyn1"        File      (rLines ["13", "130"])
  , mkTest "dyn2"        File      (rLines ["144"])
  ]

staticTests =
  [ mkTest "err-shadow-bind"  File  (staticError "shadow binding")
  , mkTest "err-arity"        File  (staticError "arity")
  , mkTest "err-not-defined"  File  (staticError "not defined")
  , mkTest "err-unbound-00"   File  (staticError "unbound")
  , mkTest "err-unbound-01"   File  (staticError "unbound")
  , mkTest "err-dup-param"    File  (staticError "duplicate parameter")
  , mkTest "err-dup-fun"      File  (staticError "duplicate function")
  , mkTest "err-large-00"    File  (staticError "too large")
  , mkTest "err-large-01"      File  (staticError "too large")
  ]

dynamicTests =
  [ mkTest "add-l"    File  (dynamicError TNumber)
  , mkTest "add-r"    File  (dynamicError TNumber)
  , mkTest "sub-l"    File  (dynamicError TNumber)
  , mkTest "sub-r"    File  (dynamicError TNumber)
  , mkTest "mul-l"    File  (dynamicError TNumber)
  , mkTest "mul-r"    File  (dynamicError TNumber)
  , mkTest "lt-l"     File  (dynamicError TNumber)
  , mkTest "lt-r"     File  (dynamicError TNumber)
  , mkTest "gt-l"     File  (dynamicError TNumber)
  , mkTest "gt-r"     File  (dynamicError TNumber)
  , mkTest "eq-l"     File  (dynamicError TNumber)
  , mkTest "eq-r"     File  (dynamicError TNumber)
  , mkTest "add1-e"   File  (dynamicError TNumber)
  , mkTest "sub1-e"   File  (dynamicError TNumber)
  , mkTest "if1"      File  (dynamicError TBoolean)
  , mkTest "if2"      File  (dynamicError TBoolean)
  , mkTest "oflow00"  File  overflowError
  , mkTest "oflow01"  File  overflowError
  ]

diamondTests =
  [ mkTest "abs"       File      (rLines ["0", "5", "7", "0"])
  , mkTest "incr"      File      (rLines ["6"])
  , mkTest "add2"      File      (rLines ["19"])
  , mkTest "fac"       File      (rLines ["5", "4", "3", "2", "1", "0", "120"])
  , mkTest "fac-tr"    File      (rLines ["120"])
  , mkTest "fib"       File      (rLines ["1", "1", "2", "3", "5", "8", "0"])
  , mkTest "even"      File      (rLines ["true", "false", "true", "false", "0"])
  ]

overflowError  = Left "Error: arithmetic overflow"
rLines         = Right . unlines
dynamicError t = Left ("Error: expected a " ++ pprint t)
staticError    = Left

anfTest :: String -> Text -> Text -> TestTree
anfTest name inS expS = testCase name $ do
  -- let res = (anfRun inS) -- `catch` (\(es :: [UserError]) -> Left (pprint es))
  check (anfRun inS) (Right expS)

anfRun :: Text -> Result
-- anfRun t = either (Left . concatMap eMsg) (Right . pprint . anormal) (parse "" t)
anfRun = Right . pprint . anormal . parse ""



--------------------------------------------------------------------------------
-- | A test program is either a filename or a text representation of source
--------------------------------------------------------------------------------
data Program = File | Code Text
type Result  = Either Text Text
--------------------------------------------------------------------------------

--------------------------------------------------------------------------------
-- | Construct a single compiler test case from a `Program`
--------------------------------------------------------------------------------
mkTest :: String -> Program -> Result -> TestTree
--------------------------------------------------------------------------------
mkTest name pgm expect = testCase name $ do
  res <- run name pgm
  check res expect


-- check :: Result -> Result -> TestTree
check (Right resV) (Right expectV) = assertEqual "Wrong result"       (trim expectV)  (trim resV)
check (Left resE)  (Left  expectE) = assertBool  "Wrong error"        (matchError expectE resE )
check (Left resE)  (Right expectV) = assertEqual "Unexpected error"   ("Value " ++ expectV) ("Error " ++ resE)
check (Right resV) (Left  expectE) = assertEqual "Unexpected result"  ("Error " ++ expectE) ("Value " ++ resV)

matchError expectE resE = (tx expectE) `isInfixOf` (tx resE)
  where
      tx = map toLower

--------------------------------------------------------------------------------
run :: FilePath -> Program -> IO Result
--------------------------------------------------------------------------------
run name pgm = do
  _ <- generateSource name pgm                 -- generate source file
  r <- executeShellCommand logF cmd timeLimit  -- compile & run
  readResult resF logF r
  where
    cmd  = printf "make %s"     resF
    resF = dirExt "output" name Res
    logF = dirExt "output" name Log

-- | `timeLimit` for each test is 15 seconds
timeLimit :: Int
timeLimit = 15 * (10 ^ 6)

--------------------------------------------------------------------------------
generateSource :: FilePath -> Program -> IO ()
--------------------------------------------------------------------------------
generateSource _    File       = return ()
generateSource name (Code pgm) = writeFile srcF pgm
  where
    srcF                       = dirExt "input"  name Src

--------------------------------------------------------------------------------
readResult :: FilePath -> FilePath -> ExitCode -> IO Result
--------------------------------------------------------------------------------
readResult resF _     ExitSuccess      = Right <$> readFile resF
readResult _    _    (ExitFailure 100) = Left  <$> return "TIMEOUT!"
readResult _    logF (ExitFailure _  ) = Left  <$> readFile logF

dirExt :: FilePath -> FilePath -> Ext -> FilePath
dirExt dir name e = "tests" </> dir </> name `ext` e
