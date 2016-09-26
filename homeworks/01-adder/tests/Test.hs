import Test.Tasty
import Test.Tasty.HUnit
import Text.Printf
import Control.Exception
import System.FilePath                  ((</>), (<.>))
import System.IO                        (withFile)
import System.Exit
import Data.List                        (isInfixOf)
import Data.Char 			                  (toLower)
import Language.Adder.Utils
import Language.Adder.Types  hiding     (Result)
import Language.Adder.Parser

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [adderTests, yourTests]

adderTests :: TestTree
adderTests = testGroup "Compiler"
  [ mkTest "forty_one"  (Code "41")               (Right "41")
  , mkTest "nyi"        (Code "let x = 10 in x")  (Right "10")
  , mkTest "five"        File                     (Right "5")
  , mkTest "adds"        File                     (Right "8")
  , mkTest "subs"        File                     (Right "8")
  , mkTest "lets"        File                     (Right "14")
  ]

yourTests :: TestTree
yourTests = testGroup "Student Tests"
  [ -- put your tests here in the format
    -- mkTest "FILE" File (Right "RESULT")
    -- or
    -- mkTest "FILE" File (Left "ERROR")
    -- where
    -- 1. you have the relevant code in tests/input/FILE.adder
    -- 2. RESULT is the desired "correct output"
    -- 3. ERROR  is a (substring of the) error string that should be generated
  ]


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

matchError e r = (tx e) `isInfixOf` (tx r)
  where
      tx       = map toLower

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
