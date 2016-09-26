{-# LANGUAGE ScopedTypeVariables #-}

import           Control.Exception  (catch)
import           System.Environment (getArgs)
import           System.IO          (stderr, hPutStrLn)
import           System.Exit
import           Language.Boa.Types
import           Language.Boa.Compiler

--------------------------------------------------------------------------------
main :: IO ()
main = runCompiler `catch` esHandle

esHandle :: [UserError] -> IO ()
esHandle es = renderErrors es >>= hPutStrLn stderr >> exitFailure

runCompiler :: IO ()
runCompiler = do
  f <- getSrcFile
  s <- readFile f
  let asm = compiler f s
  putStrLn asm
  exitSuccess

getSrcFile :: IO Text
getSrcFile = do
  args <- getArgs
  case args of
    [f] -> return f
    _   -> error "Please run with a single file as input"
