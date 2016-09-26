{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Diamondback.Types
  (
  -- * Re-Export SourceSpans
    module Language.Diamondback.UX

  -- * Abstract syntax of (a small subset of) x86 assembly instructions
  , Instruction (..)
  , Arg (..)
  , Reg (..)
  , Size (..)

  -- * Aliases for various identifiers
  , Id
  , Tag

  -- * Abstract syntax of the Adder language
  , Program (..), BareProgram , AnfProgram
  , Decl (..)   , BareDecl    , AnfDecl
  , Bind (..)   , BareBind
  , Expr (..)   , Bare        , AnfExpr, ImmExpr
  , Prim1 (..)
  , Prim2 (..)
  , isAnf

  -- * Smart Constructors
  , bindsExpr

  -- * Destructors
  , exprBinds
  , bindId
  , getLabel

  -- * Environments
  , Env
  , emptyEnv
  , pushEnv
  , lookupEnv
  , memberEnv
  , addEnv
  , fromListEnv
  , envMax
  , envArity

  -- * Dynamic Errors
  , DynError (..)
  , Ty (..)

  -- * Code Labels
  , Label (..)

  -- * Abstract Text Type
  , Ext (..)
  , ext

  ) where

import           Prelude
import qualified Data.List        as L
import           Data.Monoid
import           Data.Maybe                       (isJust)
import           Text.Printf
import           System.FilePath                  ((<.>))
import           Language.Diamondback.UX

data Reg
  = EAX
  | EBX
  | ESP
  | EBP
  | ESI

data Size
  = DWordPtr
  | WordPtr
  | BytePtr

data Arg
  = Const     Int
  | HexConst  Int
  | Reg            Reg
  | RegOffset Nat  Reg
  | RegIndex  Reg  Reg
  | Sized     Size Arg

type Nat      = Int

-- | Control-Flow Labels (New)
data Label
  = BranchTrue Tag
  | BranchDone Tag
  | DefFun     Id
  | DynamicErr DynError
  | Builtin    Text
  deriving (Show)

-- | Machine (x86) Instructions
data Instruction
  = IMov    Arg   Arg
  | IAdd    Arg   Arg
  | ISub    Arg   Arg
  | IMul    Arg   Arg
  | IShr    Arg   Arg
  | ISar    Arg   Arg
  | IShl    Arg   Arg
  | IAnd    Arg   Arg
  | IOr     Arg   Arg
  | IXor    Arg   Arg
  | ILabel  Label
  | IPush   Arg
  | IPop    Arg
  | ICmp    Arg   Arg
  | IJe     Label
  | IJne    Label
  | IJg     Label
  | IJl     Label
  | IJo     Label
  | IJmp    Label
  | ICall   Label
  | IRet

--------------------------------------------------------------------------------
-- | Abstract syntax of the Adder language
--------------------------------------------------------------------------------

-- | `Id` are program variables
type Id = Text

-- | `Tag` are used to tag each `If`
type Tag = Int

-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  | Print
  | IsNum
  | IsBool
  deriving (Show)

-- | `Prim2` are binary operations
data Prim2
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
  deriving (Show)

-- | Expr are single expressions
data Expr a
  = Number  !Integer                       a
  | Boolean !Bool                          a
  | Id      !Id                            a
  | Prim1   !Prim1    !(Expr a)            a
  | Prim2   !Prim2    !(Expr a)  !(Expr a) a
  | If      !(Expr a) !(Expr a)  !(Expr a) a
  | Let     !(Bind a) !(Expr a)  !(Expr a) a
  | App     !Id       [Expr a]             a
    deriving (Show, Functor)

-- | Bind represent the let- or function-params.

data Bind a
  = Bind !Id a
    deriving (Show, Functor)

-- | Decl are function definitions
data Decl a = Decl
  { fName  :: !(Bind a)
  , fArgs  :: [Bind a]
  , fBody  :: !(Expr a)
  , fLabel :: a
  }
  deriving (Functor, Show)

{-@ data Decl <p :: Expr a -> Prop> a = Decl
      { fName  :: Bind a
      , fArgs  :: [Bind a]
      , fBody  :: (Expr a)<p>
      , fLabel :: a
      }
  @-}

-- | A Program is a list of declarations and "main" Expr
data Program a = Prog
  { pDecls :: [Decl a]
  , pBody  :: !(Expr a)
  }
  deriving (Functor, Show)

{-@ data Program <p :: Expr a -> Prop> a = Prog
      { pDecls :: [Decl<p> a]
      , pBody  :: (Expr a)<p>
      }
  @-}


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
getLabel (Boolean _ l)   = l
getLabel (Id _ l)        = l
getLabel (Prim1 _ _ l)   = l
getLabel (Prim2 _ _ _ l) = l
getLabel (If    _ _ _ l) = l
getLabel (Let _ _ _ l)   = l
getLabel (App _ _ l)     = l

--------------------------------------------------------------------------------
-- | Dynamic Errors
--------------------------------------------------------------------------------

-- | DynError correspond to different kind of dynamic/run-time errors
data DynError
  = TypeError Ty
  | ArithOverflow
  deriving (Show)

-- | Ty correspond to two kinds of values
data Ty
  = TNumber
  | TBoolean
  deriving (Show)

--------------------------------------------------------------------------------
-- | Pretty Printer
--------------------------------------------------------------------------------
instance PPrint Ty where
  pprint TNumber  = "number"
  pprint TBoolean = "boolean"

instance PPrint Prim1 where
  pprint Add1   = "add1"
  pprint Sub1   = "sub1"
  pprint Print  = "print"
  pprint IsNum  = "isNum"
  pprint IsBool = "isBool"

instance PPrint Prim2 where
  pprint Plus    = "+"
  pprint Minus   = "-"
  pprint Times   = "*"
  pprint Less    = "<"
  pprint Greater = ">"
  pprint Equal   = "=="

ppBool :: Bool -> Text
ppBool True  = "true"
ppBool False = "false"

instance PPrint (Bind a) where
  pprint (Bind x _) = x

instance PPrint (Expr a) where
  pprint (Number n _)    = show n
  pprint (Boolean b _)   = ppBool b
  pprint (Id x _)        = x
  pprint (Prim1 o e _)   = printf "%s(%s)"               (pprint o)   (pprint e)
  pprint (Prim2 o l r _) = printf "%s %s %s"             (pprint l)   (pprint o) (pprint r)
  pprint (If    c t e _) = printf "(if %s: %s else: %s)" (pprint c)   (pprint t) (pprint e)
  pprint e@(Let {})      = printf "(let %s in %s)"       (ppBinds bs) (pprint b) where (bs, b) = exprBinds e
  pprint (App f es _)    = printf "%s(%s)"               f            (pprintMany es)

instance PPrint (Decl a) where
  pprint (Decl f xs e _) = printf "def %s(%s):\n%s" (pprint f) (pprintMany xs) body
    where
      body               = nest 4 (pprint e)

instance PPrint (Program a) where
  pprint (Prog ds e)     = L.intercalate "\n" (fmap pprint ds <> [pprint e])

nest       :: Int -> Text -> Text
nest n     = unlines . fmap pad . lines
  where
    pad s  = blanks <> s
    blanks = replicate n ' '

pprintMany :: (PPrint a) => [a] -> Text
pprintMany xs = L.intercalate ", " (fmap pprint xs)

ppBinds :: [(Bind a, Expr a)] -> Text
ppBinds bs = L.intercalate ", " [ printf "%s = %s" (pprint x) (pprint v) | (x, v) <- bs ]


--------------------------------------------------------------------------------
-- | `isAnf e` is True if `e` is an A-Normal Form
--------------------------------------------------------------------------------
{-@ measure isAnf @-}
isAnf :: Expr a -> Bool
isAnf (Number  _ _)    = True
isAnf (Boolean _ _)    = True
isAnf (Id      _ _)    = True
isAnf (Prim1 _ e _)    = isImm e
isAnf (Prim2 _ e e' _) = isImm e && isImm e'
isAnf (If c t e _)     = isImm c && isAnf t && isAnf e
isAnf (Let _ e e' _)   = isAnf e && isAnf e'
isAnf (App _ es _)     = all isAnf es

{-@ measure isImm @-}
isImm :: Expr a -> Bool
isImm (Number  _ _) = True
isImm (Boolean _ _) = True
isImm (Id      _ _) = True
isImm _             = False

{-@ type AnfExpr a = {v:Expr a| isAnf v} @-}
type AnfExpr = Expr

{-@ type ImmExpr a = {v:Expr a | isImm v} @-}
type ImmExpr = Expr

{-@ type AnfDecl a = Decl<{\e -> isAnf e}> a @-}
type AnfDecl    = Decl

{-@ type AnfProgram a = Program<{\e -> isAnf e}> @-}
type AnfProgram = Program

--------------------------------------------------------------------------------
-- | The `Bare` types are for parsed ASTs.
--------------------------------------------------------------------------------

type Bare     = Expr SourceSpan
type BareBind = Bind SourceSpan

type BareProgram = Program SourceSpan
type BareDecl    = Decl    SourceSpan

instance Located Bare where
  sourceSpan = getLabel

instance Located BareBind where
  sourceSpan (Bind _ l) = l

--------------------------------------------------------------------------------
-- | Functions for accessing the "environment" (stack)
--------------------------------------------------------------------------------

-- | An `Env` is a lookup-table mapping `Id` to some Int value
data Env = Env { envBinds :: [(Id, Int)]  -- ^ map from local vars to stack-positions
               , envMax   :: !Int         -- ^ largest value in range of `envBinds`
               , envArity :: !Int         -- ^ number of "parameters" of current fun
               }
           deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] 0 0


lookupEnv :: Id -> Env -> Maybe Int
lookupEnv k env = lookup k (envBinds env)

memberEnv :: Id -> Env -> Bool
memberEnv k env = isJust (lookupEnv k env)

pushEnv :: Bind a -> Env -> (Int, Env)
pushEnv x (Env bs n a) = (n', Env bs' n' a)
  where
    bs'                = (bindId x, n') : bs
    n'                 = 1 + n

addEnv :: Bind a -> Env -> Env
addEnv x env = snd (pushEnv x env)

fromListEnv :: [(Id, Int)] -> Env
fromListEnv bs = Env bs n (length bs)
  where
    n          = maximum (0 : [i | (_, i) <- bs])

--------------------------------------------------------------------------------
-- | File Extensions
--------------------------------------------------------------------------------

data Ext = Src    -- ^ source
         | Asm    -- ^ ascii  assembly
         | Exe    -- ^ x86    binary
         | Res    -- ^ output of execution
         | Log    -- ^ compile and execution log

instance Show Ext where
  show Src = "diamondback"
  show Asm = "s"
  show Exe = "run"
  show Res = "result"
  show Log = "log"

ext :: FilePath -> Ext -> FilePath
ext f e = f <.> show e
