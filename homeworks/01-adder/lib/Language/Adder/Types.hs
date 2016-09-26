{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Adder.Types
  (
  -- * Re-Export SourceSpans
    module Language.Adder.UX

  -- * Abstract syntax of (a small subset of) x86 assembly instructions
  , Instruction (..)
  , Arg (..)
  , Reg (..)

  -- * Aliases for various identifiers
  , Id

  -- * Abstract syntax of the Adder language
  , Bind (..)   , BareBind
  , Expr (..)   , Bare
  , Prim1 (..)

  -- * Smart Constructors
  , bindsExpr

  -- * Destructors
  , exprBinds
  , bindId

  -- * Environments
  , Env
  , emptyEnv
  , pushEnv
  , lookupEnv

  -- * Abstract Text Type
  , Ext (..)
  , ext
  ) where

import           Prelude
import qualified Data.List        as L
import           Text.Printf
import           System.FilePath                  ((<.>))
import           Language.Adder.UX

data Reg
  = EAX
  | ESP

data Arg
  = Const     Int
  | Reg            Reg
  | RegOffset Nat  Reg

type Nat      = Int

-- | Machine (x86) Instructions
data Instruction
  = IMov    Arg   Arg
  | IAdd    Arg   Arg
  | IRet

--------------------------------------------------------------------------------
-- | Abstract syntax of the Adder language
--------------------------------------------------------------------------------

-- | `Id` are program variables
type Id = Text

-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  deriving (Show)

-- | Expr are single expressions
data Expr a
  = Number  !Integer                       a
  | Prim1   !Prim1    !(Expr a)            a
  | Let     !(Bind a) !(Expr a)  !(Expr a) a
  | Id      !Id                            a
    deriving (Show, Functor)

-- | `Bind` represent the let-binders (and later, function-params.)

data Bind a
  = Bind !Id a
    deriving (Show, Functor)

bindId :: Bind a -> Id
bindId (Bind x _) = x

-- | Constructing `Expr` from let-binds
bindsExpr :: [(Bind a, Expr a)] -> Expr a -> a -> Expr a
bindsExpr bs e l = foldr (\(x, e1) e2  -> Let x e1 e2 l) e bs

-- | Destructing `Expr` into let-binds
exprBinds :: Expr a -> ([(Bind a, Expr a)], Expr a)
exprBinds (Let x e e' _) = ((x, e) : bs, body)
  where
    (bs, body)           = exprBinds e'
exprBinds body           = ([]        , body)

--------------------------------------------------------------------------------
getLabel :: Expr a -> a
--------------------------------------------------------------------------------
getLabel (Number _ l)    = l
getLabel (Id _ l)        = l
getLabel (Prim1 _ _ l)   = l
getLabel (Let _ _ _ l)   = l

--------------------------------------------------------------------------------
-- | Pretty Printer
--------------------------------------------------------------------------------
instance PPrint Prim1 where
  pprint Add1   = "add1"
  pprint Sub1   = "sub1"

instance PPrint (Bind a) where
  pprint (Bind x _) = x

instance PPrint (Expr a) where
  pprint (Number n _)    = show n
  pprint (Id x _)        = x
  pprint (Prim1 o e _)   = printf "%s(%s)"               (pprint o)   (pprint e)
  pprint e@(Let {})      = printf "(let %s in %s)"       (ppBinds bs) (pprint b) where (bs, b) = exprBinds e

ppBinds :: [(Bind a, Expr a)] -> Text
ppBinds bs = L.intercalate ", " [ printf "%s = %s" (pprint x) (pprint v) | (x, v) <- bs ]

--------------------------------------------------------------------------------
-- | The `Bare` types are for parsed ASTs.
--------------------------------------------------------------------------------
type Bare     = Expr SourceSpan

type BareBind = Bind SourceSpan

instance Located Bare where
  sourceSpan = getLabel

instance Located BareBind where
  sourceSpan (Bind _ l) = l

--------------------------------------------------------------------------------
-- | Functions for accessing the "environment" (stack)
--------------------------------------------------------------------------------

-- | An `Env` is a lookup-table mapping `Id` to some Int value
data Env = Env { envBinds :: [(Id, Int)]
               , _envMax  :: !Int
               }
           deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] 0

lookupEnv :: Id -> Env -> Maybe Int
lookupEnv k env = lookup k (envBinds env)

pushEnv :: Bind a -> Env -> (Int, Env)
pushEnv x (Env bs n) = (n', Env bs' n')
  where
    bs'              = (bindId x, n') : bs
    n'               = 1 + n

--------------------------------------------------------------------------------
-- | File Extensions
--------------------------------------------------------------------------------

data Ext = Src    -- ^ source
         | Asm    -- ^ ascii  assembly
         | Exe    -- ^ x86    binary
         | Res    -- ^ output of execution
         | Log    -- ^ compile and execution log

instance Show Ext where
  show Src = "adder"
  show Asm = "s"
  show Exe = "run"
  show Res = "result"
  show Log = "log"

ext :: FilePath -> Ext -> FilePath
ext f e = f <.> show e
