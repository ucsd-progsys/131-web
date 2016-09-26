module Language.Adder.Asm (asm) where

import           Data.Monoid
import qualified Data.List as L
import           Text.Printf (printf)
import           Language.Adder.Types

--------------------------------------------------------------------------------
-- | Convert a sequence of x86 `Instructions` into the output assembly
--------------------------------------------------------------------------------
asm :: [Instruction] -> Text
--------------------------------------------------------------------------------
asm instrs = header <> instrsAsm (instrs) <> "\n"

instrsAsm :: [Instruction] -> Text
instrsAsm = L.intercalate "\n" . map instrAsm

header :: Text
header = unlines
  [ "section .text"
  , "extern error"
  , "extern print"
  , "global our_code_starts_here"
  , "our_code_starts_here:"
  ]

--------------------------------------------------------------------------------
instrAsm :: Instruction -> Text
--------------------------------------------------------------------------------
instrAsm (IMov dst val) = printf "  mov %s, %s"  (argAsm dst) (argAsm val)
instrAsm (IAdd dst val) = error  "TBD"
instrAsm IRet           =        "  ret"

regAsm :: Reg -> Text
regAsm EAX = "eax"
regAsm ESP = error "TBD"

argAsm :: Arg -> Text
argAsm (Const n)       = printf "%d" n
argAsm (Reg r)         = regAsm r
argAsm (RegOffset n r) = error "TBD"
