import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Control.Exception
import System.FilePath                  ((</>), (<.>))
import System.IO                        (withFile)
import System.Exit
import Data.List                        (isInfixOf)
import Data.Char 			(toLower)
import Language.Boa.Utils
import Language.Boa.Types  hiding       (Result)
import Language.Boa.Parser
import Language.Boa.Normalizer

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests"
  [ anfTests
  , yourAnfTests
  , compilerTests
  , yourCompilerTests
  ]



anfTests:: TestTree
anfTests = testGroup "Normalizer"
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

compilerTests :: TestTree
compilerTests = testGroup "Compiler"
  [ mkTest "forty_one"  (Code "41")               (Right "41")
  , mkTest "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest "five"        File                     (Right "5")
  , mkTest "adds"        File                     (Right "8")
  , mkTest "subs"        File                     (Right "8")
  , mkTest "lets"        File                     (Right "14")
  , mkTest "expr0"       File                     (Right "600")
  , mkTest "expr1"       File                     (Right "30")
  , mkTest "if1"         File                     (Right "4")
  , mkTest "if2"         File                     (Right "2")
  , mkTest "expr2"       File                     (Right "20")
  , mkTest "expr3"       File                     (Right "20")
  , mkTest "expr4"       File                     (Right "-8")
  ]

yourAnfTests :: TestTree
yourAnfTests = testGroup "Your-Normalizer"
  [ -- TBD following the format of each test in `anfTests`
  ]

yourCompilerTests :: TestTree
yourCompilerTests = testGroup "Your-Compiler"
  [ -- TBD following the format of each test in `compilerTests`
  ]

anfTest :: String -> Text -> Text -> TestTree
anfTest name inS expS = testCase name $ do
  check (anfRun inS) (Right expS)

anfRun :: Text -> Result
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
