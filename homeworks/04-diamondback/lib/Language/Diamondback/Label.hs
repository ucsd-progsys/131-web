module Language.Diamondback.Label (tag, tails) where

import           Language.Diamondback.Types
import qualified Data.List as L

--------------------------------------------------------------------------------
-- | Transformation to mark tail-call positions
--------------------------------------------------------------------------------
tails :: Program a -> Program (a, Bool)
--------------------------------------------------------------------------------
tails (Prog ds e) = Prog ds' e'
  where
    ds' = [tailsD d | d <- ds]
    e'  = tailsE Nothing e

tailsD :: Decl a -> Decl (a, Bool)
tailsD (Decl f xs e l) = Decl f' xs' e' (l, False)
  where
    f' : xs'           = tailsBind <$> f: xs
    e'                 = tailsE (Just (bindId f)) e

tailsE :: Maybe Id -> Expr a -> Expr (a, Bool)
tailsE g = go True
  where
    go _ (Number n l)      = tailsTop l (Number n)

    go _ (Boolean b l)     = tailsTop l (Boolean b)

    go _ (Id     x l)      = tailsTop l (Id x)

    go _ (Prim1 o e1 l)    = tailsTop l (Prim1 o e1')
      where
        e1'                = go False e1

    go _ (Prim2 o e1 e2 l) = tailsTop  l (Prim2 o e1' e2')
      where
        [e1', e2']         = go False <$> [e1, e2]

    go b (If c e1 e2 l)    = tailsTop l (If c' e1' e2')
      where
        c'                 = go False c
        e1'                = go b     e1
        e2'                = go b     e2

    go b (Let x e1 e2 l)   = tailsTop l (Let x' e1' e2')
      where
        e1'                = go False e1
        e2'                = go b     e2
        x'                 = tailsBind x

    go b (App f es l)      = App f es' (l, b && g == Just f)
      where
        es'                = go False <$> es

tailsBind :: Bind a -> Bind (a, Bool)
tailsBind (Bind x l)   = tailsTop l (Bind x)

tailsTop :: a -> ((a, Bool) -> b) -> b
tailsTop l f = f (l, False)

--------------------------------------------------------------------------------
-- | Transformation to ensure each sub-expression gets a distinct tag
--------------------------------------------------------------------------------
tag :: Program a -> Program (a, Tag)
--------------------------------------------------------------------------------
tag (Prog ds e) = Prog ds' e'
  where
    (i', ds')     = L.mapAccumL tagD 1  ds
    (_ , e')      =             tagE i' e

--------------------------------------------------------------------------------
tagD :: Int -> Decl a -> (Int, Decl (a, Tag))
--------------------------------------------------------------------------------
tagD i (Decl f xs e l) = tagTop i'' l (Decl f' xs' e')
  where
    (i', e')             = tagE i e
    (i'', f':xs')        = L.mapAccumL tagBind i' (f:xs)

--------------------------------------------------------------------------------
tagE :: Int -> Expr a -> (Int, Expr (a, Tag))
--------------------------------------------------------------------------------
tagE = go
  where
    go i (Number n l)      = tagTop i  l (Number n)

    go i (Boolean b l)     = tagTop i  l (Boolean b)

    go i (Id     x l)      = tagTop i  l (Id x)

    go i (Prim1 o e1 l)    = tagTop i' l (Prim1 o e1')
      where
        (i', e1')          = go i e1

    go i (Prim2 o e1 e2 l) = tagTop i'' l (Prim2 o e1' e2')
      where
        (i',  e1')         = go i  e1
        (i'', e2')         = go i' e2

    go i (If c e1 e2 l)    = tagTop i''' l (If c' e1' e2')
      where
        (i'  , c' )        = go i   c
        (i'' , e1')        = go i'  e1
        (i''', e2')        = go i'' e2

    go i (Let x e b l)     = tagTop i'' l (Let x' e' b')
      where
        (i', [e', b'])     = L.mapAccumL go i [e, b]
        (i'', x')          = tagBind i' x

    go i (App f es l)      = tagTop i' l (App f es')
      where
        (i', es')       = L.mapAccumL go i es

tagTop :: Tag -> a -> ((a, Tag) -> b) -> (Tag, b)
tagTop i l c             = (i + 1, c (l, i))

tagBind :: Tag -> Bind a -> (Tag, Bind (a, Tag))
tagBind i (Bind x l)     = tagTop i l (Bind x)
