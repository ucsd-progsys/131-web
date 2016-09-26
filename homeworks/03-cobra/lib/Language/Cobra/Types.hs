{-# LANGUAGE DeriveFunctor        #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances    #-}

module Language.Cobra.Types
  (
  -- * Re-Export SourceSpans
    module Language.Cobra.UX

  -- * Abstract syntax of (a small subset of) x86 assembly instructions
  , Instruction (..)
  , Arg (..)
  , Reg (..)
  , Size (..)

  -- * Aliases for various identifiers
  , Id
  , Tag

  -- * Abstract syntax of the Adder language
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

  -- * Labels
  , label
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
  -- , insertEnv

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
import           Data.Maybe                       (isJust)
import           Text.Printf
import           System.FilePath                  ((<.>))
import           Language.Cobra.UX

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
  | Let     !(Bind a) !(Expr a)  !(Expr a) a
  | If      !(Expr a) !(Expr a)  !(Expr a) a
    deriving (Show, Functor)

-- | Bind represent the let- or function-params.

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
getLabel (Boolean _ l)   = l
getLabel (Id _ l)        = l
getLabel (Prim1 _ _ l)   = l
getLabel (Prim2 _ _ _ l) = l
getLabel (If    _ _ _ l) = l
getLabel (Let _ _ _ l)   = l

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
  pprint e@Let {}        = printf "(let %s in %s)"       (ppBinds bs) (pprint b) where (bs, b) = exprBinds e

ppBinds :: [(Bind a, Expr a)] -> Text
ppBinds bs = L.intercalate ", " [ printf "%s = %s" (pprint x) (pprint v) | (x, v) <- bs ]


--------------------------------------------------------------------------------
-- | Transformation to ensure each sub-expression gets a distinct tag
--------------------------------------------------------------------------------
label :: Expr a -> Expr (a, Tag)
--------------------------------------------------------------------------------
label = snd . go 0
  where
    go i (Number n l)      = labelTop i  l (Number n)

    go i (Boolean b l)     = labelTop i  l (Boolean b)

    go i (Id     x l)      = labelTop i  l (Id x)

    go i (Prim1 o e1 l)    = labelTop i' l (Prim1 o e1')
      where
        (i', e1')          = go i e1

    go i (Prim2 o e1 e2 l) = labelTop i'' l (Prim2 o e1' e2')
      where
        (i',  e1')         = go i  e1
        (i'', e2')         = go i' e2

    go i (If c e1 e2 l)    = labelTop i''' l (If c' e1' e2')
      where
        (i'  , c' )        = go i   c
        (i'' , e1')        = go i'  e1
        (i''', e2')        = go i'' e2

    go i (Let x e b l)     = labelTop i'' l (Let x' e' b')
      where
        (i', [e', b'])     = L.mapAccumL go i [e, b]
        (i'', x')          = labelBind i' x

labelTop :: Tag -> a -> ((a, Tag) -> b) -> (Tag, b)
labelTop i l c             = (i + 1, c (l, i))

labelBind :: Tag -> Bind a -> (Tag, Bind (a, Tag))
labelBind i (Bind x l)     = labelTop i l (Bind x)

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

--------------------------------------------------------------------------------
-- | The `Bare` types are for parsed ASTs.
--------------------------------------------------------------------------------

type Bare     = Expr SourceSpan
type BareBind = Bind SourceSpan


instance Located a => Located (Expr a) where
  sourceSpan = sourceSpan . getLabel

instance Located Bare where
  sourceSpan = getLabel

instance Located BareBind where
  sourceSpan (Bind _ l) = l

--------------------------------------------------------------------------------
-- | Functions for accessing the "environment" (stack)
--------------------------------------------------------------------------------

-- | An `Env` is a lookup-table mapping `Id` to some Int value
data Env = Env { envBinds :: [(Id, Int)]
               , envMax   :: !Int
               }
           deriving (Show)

emptyEnv :: Env
emptyEnv = Env [] 0

lookupEnv :: Id -> Env -> Maybe Int
lookupEnv k env = lookup k (envBinds env)

memberEnv :: Id -> Env -> Bool
memberEnv k env = isJust (lookupEnv k env)

pushEnv :: Bind a -> Env -> (Int, Env)
pushEnv x (Env bs n) = (n', Env bs' n')
  where
    bs'              = (bindId x, n') : bs
    n'               = 1 + n

addEnv :: Bind a -> Env -> Env
addEnv x env = snd (pushEnv x env)

fromListEnv :: [(Id, Int)] -> Env
fromListEnv bs = Env bs n
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
  show Src = "cobra"
  show Asm = "s"
  show Exe = "run"
  show Res = "result"
  show Log = "log"

ext :: FilePath -> Ext -> FilePath
ext f e = f <.> show e
