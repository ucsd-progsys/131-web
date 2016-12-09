---
title: Static Type Inference
date: 2016-11-30
headerImg: garter-snake.jpg
---

## Type Inference

Garter, aka **G**uaranteed **T**ype **E**nforced **R**eliability, is an
extensnion of FDL with **statically inferred types**.

`garter` starts with `fer-de-lance` and makes
one major addition and a minor deletion, in particular, we

* add **static types**,
* replace unbounded tuples, with **pairs**.

That is, we now have a proper type system and the `Checker`
is extended to **infer** types for all sub-expressions.

The code proceeds to `compile` (i.e. `Asm` generation)
**only if it type checks**.

This lets us eliminate a whole bunch of the **dynamic tests**

- `assertType TNumber`
- `assertType TBool`
- `assertType TTuple`
- `assertBound`

etc. as code that typechecks is *guaranteed* to pass the checks at run time.

## Strategy

Lets start with an informal overview of our strategy for type inference;
as usual we will then formalize and implement this strategy.

The core idea is this:

1. **Traverse** the `Expr` ...
2. **Generating** fresh variables for unknown types...
3. **Unifying** function input types with their arguments ...
4. **Substituting** solutions for variables to infer types.

Lets do the above procedure informally for a couple of examples!

### Example 1: Inputs and Outputs

```haskell
let incr = (lambda(x): add1(x)) in
  incr(7)  
```

### Example 2: Polymorphism

```haskell
let id = (lambda(x): x)
  , a1 = id(7)
  , a2 = id(true)
in
  true  
```

### Example 3: Higher-Order Functions

```haskell
let f    = (lambda(it, x): it(x) + 1)
  , incr = lambda(z): add1(z)
in
  f(incr, 10)
```

### Strategy Recap

1. **Traverse** the `Expr` ...
2. **Fresh** variables for unknown types...
3. **Unifying** function input types with their arguments ...
4. **Substituting** solutions for variables to infer types ...
5. **Generalizing** types into polymorphic functions ...
6. **Instantiating** polymorphic type variables at each use-site.

## Plan

1. [**Types**](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [Variables & Substitution](#substitutions)
4. [Unification](#unification)
5. [Generalize & Instantiate](#generalize-and-instantiate)
6. [Inferring Types](#inference)
7. [Extensions](#extensions)

## Syntax

First, lets see how the syntax of our `garter` changes to
enable static types.

### Syntax of Types

A `Type` is one of:

```haskell
data Type
  = TInt               -- Int
  | TBool              -- Bool
  | [Type] :=> Type    -- (t1,...,tn) => t
  | TVar TVar          -- a, b, c
  | TPair Type Type    -- (t0, t1)
  | TCtor Ctor [Type]  -- e.g. C[t1,...,tn]
  deriving (Eq, Ord)
```

here `Ctor` and `TVar` are just string names:

```haskell
newtype Ctor
  = CT String           -- e.g. "List", "Tree"
  deriving (Eq, Ord)

newtype TVar
  = TV String           -- e.g. a, b, c
  deriving (Eq, Ord)
```

Finally, a **polymorphic type** is represented as:

```haskell
data Poly = Forall [TVar] Type -- e.g. forall a. (a, a) => Bool
```

### Example: Monomorphic Types

A function that

* takes two input `Int`
* returns an output `Int`

Has the *monomorphic* type `(Int, Int) => Int`

Which we would represent as a `Poly` value:

```
  (Forall [] ([TInt, TInt] :=> TInt))          :: Poly
```

**Note:**
If a function is **monomorphic** (i.e. *not polymorphic*),
we can just use the empty list of `TVar`.

### Example: Polymorphic Types

Similarly, a function that

* takes a value of **any** type and
* returns a value of **the same** type

Has the *polymorphic* type `forall a. (a) => a`

Which we would represent as a `Poly` value:

```
  Forall [TV "a"] ([TVar (TV "a")] :=> (TVar (TV "a")))
```

**Note:** Haskell allows `OverloadedStrings` which
lets us treat `"a"` as both a `TVar` and a `Type`
as needed. So we can rewrite the above as:

```
  Forall ["a"] (["a"] :=> "a")
```

Similarly, a function that takes two values and returns the first,
can be given a `Poly` type `forall a, b. (a, b) => a` which is
represented as:


```
  Forall ["a", "b"] (["a", "b"] :=> "a")
```

### Syntax of Expressions

To enable inference `garter` simplifies the language a little bit.

+ *Dynamic tests* `isNum` and `isBool` are removed,

+ *Tuples* always have exactly *two* elements;
  you can represent `(x, y, z)` as `(x, (y, z))`.

+ *Tuple* access is limited to the fields `Zero`
  and `One` (instead of arbitrary expressions).

```haskell
data Expr a
  = ...
  | Tuple   (Expr a) (Expr a)                    a -- Tuples have 2 elems
  | GetItem (Expr a) Field                       a -- Access 0-th or 1-st elem
  | Fun     (Bind a) Sig     [Bind a]  (Expr a)  a -- named functions

data Field
  = Zero            -- 0-th field
  | One             -- 1-st field                     

data Sig
  = Infer           -- no specification, infer scheme  
  | Check  Poly     -- check that function has given (poly-) type
  | Assume Poly     --  
```

### Plan

1. [Types](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [**Variables & Substitution**](#substitutions)
4. [Unification](#unification)
5. [Generalize & Instantiate](#generalize-and-instantiate)
6. [Inferring Types](#inference)
7. [Extensions](#extensions)

## Substitutions

Our informal algorithm proceeds by

* Generating **fresh type** variables for unknown types,
* Traversing the `Expr` to **unify** the types of sub-expressions,
* By **substituting** a type *variable* with a whole *type*.

Lets formalize *substitutions*, and use it to define [unification](#unification).

### Representing Substitutions

We represent substitutions as a record with two fields:

```haskell
data Subst = Su
  { suMap :: M.Map TVar Type    -- ^ hashmap from type-var := type
  , suCnt :: !Int               -- ^ counter used to generate fresh type vars
  }
```

* `suMap` is a **map** from type variables to types,
* `suCnt` is a **counter** used to generate **new** type variables.

For example, `exSubst` is a substitution that maps `a`, `b` and `c` to
`Int`, `Bool` and `(Int, Int) => Int` respectively.

```haskell
exSubst :: Subst
exSubst = Su m 0
  where
    m   = (M.fromList [ ("a", TInt)
                      , ("b", TBool)
                      , ("c", [TInt, TInt] :=> TInt) ])


apply exSubst ([Int, "zong"] :=> Bool)


```

### Applying Substitutions

The main *use* of a substition is to **apply** it to a type, which
has the effect of *replacing* each occurrence of a type variable
with its substituted value (or leaving it untouched if it is not
mentioned in the substitution.)

We define an interface for "things that can be substituted" as:

```haskell
class Substitutable a where
  apply     :: Subst -> a -> a
```

and then we define how to `apply` substitutions to `Type`, `Poly`,
and lists and maps of `Type` and `Poly`.

For example,

```haskell
apply exSubst (["a", "z"] :=> "b")
```

returns

```haskell
[TInt, "z"] :=> TBool
```

by replacing `"a"` and `"b"` with `TInt` and `TBool` and leaving "z" alone.

### QUIZ

Recall that `exSubst = ["a" := TInt, "b" := TBool]`...

What should be the result of

```
apply exSubst (Forall "a" (["a"] :=> "a"))
```

1. `Forall ["a"] ([TInt] :=> TBool)`
2. `Forall ["a"] (["a" ] :=> "a"  )`
3. `Forall ["a"] (["a"]  :=> TBool)`
4. `Forall ["a"] ([TInt] :=> "a")`
5. `Forall [   ] ([TInt] :=> TBool)`


### Bound vs. Free Type Variables

Indeed, the type

```
forall a. (a) => a
```

is identical to

```
forall z. (z) => a
```

+ A **bound** type variable is one that appears under a `forall`.

+ A **free** type variable is one that is **not** bound.

We should only substitute **free type variables**.

### Applying Substitutions

Thus, keeping the above in mind, we can define `apply` as a recursive traversal:

```haskell
  apply _  TInt          = TInt
  apply _  TBool         = TBool
  apply su (TVar a)      = M.findWithDefault (TVar a) a (suMap su)
  apply su (ts :=> t)    = apply su ts :=> apply su t
  apply su (Forall as t) = Forall as $ apply (unSubst as su)  t
```

where `unSubst` **removes** the mappings for `as` from `su`

### Creating Substitutions

We can start with an **empty substitution** that maps no variables

```haskell
empSubst :: Subst
empSubst =  Su M.empty 0
```

### Extending Substitutions

we can **extend** the substitution by assigning a variable `a` to type `t`

```haskell
extSubst :: Subst -> TVar -> Type -> Subst
extSubst su a t = su { suMap = M.insert a t su' }
  where
     su'        = apply (mkSubst [(a, t)]) (suMap su)
```

**Telescoping**

Note that when we extend `[b := a]` by assigning `a` to `Int` we must
take care to also update `b` to now map to `Int`. That is, we want:

```
    extSubst [ "b" := "a" ] "a" TInt
```

to be

```
    [ "b" := TInt, "a" := TInt ]
```

That is why we:

1. `apply [a := Int]` to update the old substitution to get `su'`
2. `insert` the new assignment `a := Int` into `su'`.


### Plan

1. [Types](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [Variables & Substitution](#substitutions)
4. [**Unification**](#unification)
5. [Generalize & Instantiate](#generalize-and-instantiate)
6. [Inferring Types](#inference)
7. [Extensions](#extensions)

## Unification

Next, lets use `Subst` to implement a procedure to `unify` two types,
i.e. to determine the conditions under which the two types are *the same*.


| **T1**     | **T2**       | **Unified**     | **Substitution**    |
|:-----------|:-------------|:----------------|:--------------------|
| `Int`      | `Int`        | `Int`           | `empSubst`          |
| `a`        | `Int`        | `Int`           | `a := Int`          |
| `a`        | `b`          | `b`             | `a := b`            |
| `a -> b`   | `a -> d`     | `a->d`          | `b := d`            |
| `a -> Int` | `Bool -> b`  | `Bool -> Int`   | `a := Bool, b:=Int` |
| `Int`      | `Bool`       | *Error*         | *Error*             |
| `Int`      | `a -> b`     | *Error*         | *Error*             |
| `a`        | `a -> Int`   | *Error*         | *Error*             |

* The first few cases: unification is possible,
* The last few cases: unification fails, i.e. type error in source!

**Occurs Check**

* The very last failure: `a` in the first type **occurs inside**
  free inside the second type!

* If we try substituting `a` with `a -> Int` we will just keep
  spinning forever! Hence, this also throws a unification failure.

**Exercise**

Can you think of a program that would trigger the *occurs check* failure?
(You've probable written several such programs in 130 and 131!)

### Implementing Unification

We implement unification as a function:

```haskell
unify :: Subst -> Type -> Type -> Subst
```

such that

```haskell
unify su t1 t2
```

returns a substitution `su'` that **extends** `su` with
with the assignments needed to make `t1` the same as `t2`,
i.e. such that `apply su' t1 == apply su' t2`.

The code is pretty much the table above:

```haskell
unify :: SourceSpan -> Subst -> Type -> Type -> Subst
unify _  su TInt       TInt         = su
unify _  su TBool      TBool        = su
unify sp su (TVar a)   t            = varAsgn sp su a t
unify sp su t          (TVar a)     = varAsgn sp su a t
unify sp su (is :=> o) (is' :=> o') = unify (unifys sp su is is') (apply s1 o) (apply s1 o')
unify sp _  t1 t2                   = abort (errUnify sp t1 t2)
```

The helpers

* `unifys` recursively calls `unify` on *lists* of types:
* `varAsgn` extends `su` with `[a := t]` if **required** and **possible**!

```haskell
varAsgn :: SourceSpan -> Subst -> TVar -> Type -> Subst
varAsgn sp su a t
  | t == TVar a          = su                       -- assigned to itself, do nothing
  | a `elem` freeTvars t = abort (errOccurs sp a t) -- oops, fails occurs check!
  | otherwise            = extSubst su a t          -- extend su with [a := t]
```

We can test out the above table:

```haskell
λ> :set -XOverloadedStrings

λ> unify junkSpan empSubst TInt TInt
(fromList [], 0)

λ> unify junkSpan empSubst (["a"] :=> TInt) ([TBool] :=> "b")
(fromList [(a,Bool), (b,Int)], 0)

λ> unify junkSpan empSubst ("a") (["a"] :=> TInt)
*** Exception: "Type error: occurs check fails: a occurs in (a) => Int"...

λ> unify junkSpan empSubst TInt TBool
*** Exception: "Type error: cannot unify Int and Bool" ...
```

### Plan

1. [Types](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [Variables & Substitution](#substitutions)
4. [Unification](#unification)
5. [**Generalize & Instantiate**](#generalize-and-instantiate)
6. [Inferring Types](#inference)
7. [Extensions](#extensions)

## Generalize and Instantiate

Ok, the next step is an easy one. Recall the example:

```haskell
let id = (lambda(x): x)
  , a1 = id(7)
  , a2 = id(true)
in
  true  
```

For the expression `lambda(x): x` we inferred the type `(a0) => a0`

We needed to **generalize** the above:

* to assign `id` the `Poly`-type: `forall a0. (a0) => a0`

We needed to **instantiate** the above `Poly`-type at each *use*

* at `id(7)`    the function `id` has `Type` `(Int)  => Int`
* at `id(true)` the function `id` has `Type` `(Bool) => Int`

Lets see how to implement those two steps.

### Type Environments

To `generalize` a type, we

1. Compute its (free) type variables,
2. Remove the ones that may still be
   constrained by *other* in-scope program variables.

We represent the types of **in scope** program variables
as **type environment**

```haskell
data TypeEnv = TypeEnv (M.Map Id Poly)
```

i.e. a `Map` from program variables `Id` to their (inferred) `Poly` type.

### Generalize

We can now implement `generalize` as:

```haskell
generalize :: TypeEnv -> Type -> Poly
generalize env t = Forall as t           -- 4. slap a `Forall` on the unconstrained vars
  where
    tvs          = freeTvars t           -- 1. compute tyVars of `t`
    evs          = freeTvars env         -- 2. compute tyVars of `env`
    as           = L.nub (tvs L.\\ evs)  -- 3. compute unconstrained vars: (1) minus (2)
```

The helper `freeTvars` computes the set of variables
that appear inside a `Type`, `Poly` and `TypeEnv`:

```haskell
  -- | type-vars of a Type
  freeTvars TInt          = []
  freeTvars TBool         = []
  freeTvars (TVar a)      = [a]
  freeTvars (ts :=> t)    = freeTvars ts ++ freeTvars t

  -- | type-vars of a Poly (we remove the "bound" forall-vars)
  freeTvars (Forall as t) = freeTvars t L.\\ as  

  -- | type-vars of an TypeEnv
  freeTvars (TypeEnv m)   = freeTvars [s | (x, s) <- M.toList m]  
```

### Instantiate

Next, to **instantiate** a `Poly` of the form

```
   Forall [a1,...,an] t
```

we:

1. Generate **fresh** type variables, `b1,...,bn` for each "parameter" `a1...an`
2. Substitute `[a1 := b1,...,an := bn]` in the "body" `t`.

For example, to instantiate

```haskell
   Forall ["a"] (["a"] :=> "a")
```

we

1. Generate a fresh variable e.g. `"a66"`,
2. Substitute `["a" := "a66"]` in the body `["a"] :=> "a"`

to get

```haskell
["a66"] :=> "a66"
```

### Implementing Instantiate

We implement the above as:

```haskell
instantiate :: Subst -> Poly -> (Subst, Type)
instantiate su (Forall as t) = (su', apply suInst t)
  where
    (su', as')               = freshTVars su n
    suInst                   = mkSubst (zip as as')
    n                        = length as
```

**Question** Why does `instantiate` **return** a `Subst`?

Lets run it and see what happens!

```haskell
λ> :set -XOverloadedStrings

λ> let tId = Forall ["a"] (["a"] :=> "a")

λ> let (su0, id0) = instantiate empSubst tId

λ> let (su1, id1) = instantiate su0      tId

λ> id0
(a0) => a0

λ> id1
(a1) => a1
```

As in `anf` and `label`

* The `fresh` calls **bump up** the counter ...
* But must use the **returned** `Subst` for future calls.


### Plan

1. [Types](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [Variables & Substitution](#substitutions)
4. [Unification](#unification)
5. [Generalize & Instantiate](#generalize-and-instantiate)
6. [**Inferring Types**](#inference)
7. [Extensions](#extensions)

## Inference

Finally, we have all the pieces to implement the actual
**type inference** procedure `ti`

```haskell
ti :: TypeEnv -> Subst -> Expr a -> (Subst, Type)
```

The function `ti` takes as *input*:

1. A `TypeEnv` mapping in-scope variables to their inferred (`Poly`)-types,
2. A `Subst` containing the *current* substitutions and fresh-variable-counter,
3. An `Expr` whose type we want to infer.

and returns as output a pair of:

+ The inferred type for the given `Expr` and
+ The extended `Subst` resulting from
    + generating fresh type variables and
    + doing the unification needed to check the `Expr`.

Lets look at how `ti` is implemented for the different cases of expressions.

### Inference: Literals

For numbers and booleans, we just return
the respective type and the input `Subst`
without any modifications.

```haskell
ti _ su   (Number {})      = (su, TInt)
ti _ su   (Boolean {})     = (su, TBool)
```

### Inference: Variables

For identifiers, we

1. **lookup** their type in the `env` and
2. **instantiate** type-variables to get *different types at different uses*.

```haskell
ti env su (Id x l) = instantiate su (lookupTypeEnv x env)
```

Why do we *instantiate*? Recall [the `id` example!](generalize-and-instantiate)

### Inference: Let-bindings

Next, lets look at let-bindings:

```haskell
ti env su (Let x e1 e2 _)  = ti env'' su1 e2         -- (5)
  where
    (su1, t1)              = ti env su  e1           -- (1)
    env'                   = apply su1  env          -- (2)
    s1                     = generalize env' t1      -- (3)
    env''                  = extTypeEnv x s1 env'    -- (4)
```

In essence,

1. **Infer** the type `t1` for `e1`,
2. **Apply** the substitutions from (1) to the `env`,
3. **Generalize** `t1` to make it a `Poly` type `s1`,
    - why? [recall the id example](#generalize-and-instantiate)
4. **Extend** the env to map `x` to `s1` and,
5. **Infer** the type of `e2` in the extended environment.

### Inference: Function Definitions

Next, lets see how to infer the type of a function i.e. `Lam`

```haskell
ti env su (Lam xs body) = (su3, apply su3 (tXs :=> tOut))   -- (5)
  where
    (su1, tXs :=> tOut) = freshFun su (length xs)           -- (1)
    env'                = extTypesEnv env (zip xs tXs)      -- (2)
    (su2, tBody)        = ti env' su1 body                  -- (3)
    su3                 = unify su2 tBody (apply su2 tOut)  -- (4)
```

Inference works as follows:

1. Generate a *function type* with fresh variables for the
   unknown inputs (`tXs`) and output (`tOut`),
2. Extend the `env` so the parameters `xs` have types `tXs`,
3. Infer the type of `body` under the extended `env'` as `tBody`,
4. Unify the *expected* output `tOut` with the *actual* `tBody`
5. Apply the substitutions to infer the function's type `tXs :=> tOut`.


### Inference: Function Calls

Finally, lets see how to infer the types of a call i.e. `App`

```haskell
ti env su (App f es) = (su4, apply su4 tOut)          -- (6)
  where
    (su1, tF)        = ti env su  f                   -- (1)
    env'             = apply  su1 env                 -- (2)
    (su2, tEs)       = L.mapAccumL (ti env') su1 es   -- (3)
    (su3, tOut)      = freshTVar su2                  -- (4)
    su4              = unify su3 tF (tEs :=> tOut)    -- (5)

```

The code works as follows:

1. Infer the type of the function `f` as `tF`,
2. Apply resulting substitutions to `env` (as before),
3. Infer the types of the inputs `es` as `tEs`,
4. Generate a variable for the unknown output type,
5. Unify the *actual* input-output `tEs :=> tOut` with the *expected* `tF`
6. Return the (substituted) `tOut` as the inferred type of the expression.

**Note:** In your starter code we *factor* out steps 3-6,
i.e. the code that checks the arguments and infers the
output type into `tiApp` to let us reuse it
to [handle extensions](#extensions)

### Plan

1. [Types](#syntax-of-types)
2. [Expressions](#syntax-of-expressions)
3. [Variables & Substitution](#substitutions)
4. [Unification](#unification)
5. [Generalize & Instantiate](#generalize-and-instantiate)
6. [Inferring Types](#inference)
7. [**Extensions**](#extensions)

## Extensions

The above gives you the basic idea, now *you* will have to
implement a bunch of extensions.

1. Primitives
2. (Recursive) Functions
3. Type Checking

### Extensions: Primitives

What about *primitives* ?

- `add1(e)`, `print(e)`, `e1 + e2` etc.

What about *branches* ?

- `if cond: e1 else: e2`

What about *tuples* ?

- `(e1, e2)` and `e[0]` and `e[1]`

All of the above can be handled as **applications** to special
functions.

For example, you can handle `add1(e)` by treating it
as passing a parameter `e` to  a function with type:

```
  (Int) => Int
```

Similarly, handle `e1 + e2` by treating it as passing the
parameters `[e1, e2]` to a function with type:

```
  (Int, Int) => Int
```

Can you figure out how to similarly account for branches, tuples, etc. by
filling in suitable implementations of:

* `prim2Poly`
* `ifPoly`
* `tupPoly`, `fieldPoly`

**HINT:**
Use the implementation for `prim1Poly` (and `instApp`, `tiApp`)
in the starter code as a guide.


### Extensions: (Recursive) Functions

Extend or modify the code for handling `Lam xs e` so that
you can handle `Fun f xs e`. This is mostly straightforward:

* You can basically reuse the code as is
* **Except** if `f` appears in the body of `e`

Can you figure out how to modify the environment under
which `e` is checked to handle the above case?

### Extensions: Type Checking

While inference is great, it is often useful to *specify* the types.

While inference is great, it is often useful to *specify* the types.

* They can describe behavior of *untyped code*
* They can be nice *documentation*, e.g. when
  we want a function to have a more *restrictive* type.

### Assuming Specifications for Untyped Code

For example, we can **implement** lists as tuples and tell the
type system to:

* **trust the implementation** of the core list library API, but
* **verify  the uses** of the list library.

We do this by:

```
def null() as forall a. () => List[a]:
  false
in

def link(h, t) as forall a. (a, List[a]) => List[a]:
  (h, t)
in

def tail(l) as forall a. (List[a]) => List[a]:
  l[1]
in

def length(l):
  if l == null():
    0
  else:
    1 + length(tail(l))
in

let l0 = link(0, link(1, link(2, null())))
in
    length(l0)
```

The `as` keyword tells the system to **trust** the signature,
i.e. to **assume** it is ok, and to **not check** the implementations
of the function (see how `ti` works for `Assume`.)

However, the signatures are **used** to ensure that `null`, `link` and `tail`
are used properly, for example, if we tried [the following](tests/input/err-list.gtr)

```
    link(1, link(true, null()))
```

we should get an error:

```
tests/input/err-list.gtr:9:1-24: Type error: cannot unify Int and Bool

         9|  link(1, link(true, null()))
             ^^^^^^^^^^^^^^^^^^^^^^^
```


### Checking Specifications

Finally, sometimes we may want to restrict a function be used
to some more *specific* type than what would be inferred.

`garter` allows for specifications on functions using the `::`
operator. For example, you may want a special function that
just compares two `Int` for equality:

```
def eqInt(x, y) :: (Int, Int) => Bool :
  x == y
in
  eqInt(17, 19)  
```

As another example, you might write a `swapList` function
that swaps **pairs of lists** The same code would
swap arbitrary pairs, but lets say you really want
it work just for lists:

```
def null() as forall a. () => List[a]:
  false
in

def link(h, t) as forall a. (a, List[a]) => List[a]:
  (h, t)
in

def swapList(p) :: forall a.((List[a], List[b])) => (List[b], List[a]) :
  (p[1], p[0])
in

let l0 = link(1, null())
  , l1 = link(true, null())
in
  swapList((l0, l1))
```

Can you figure out how to extend the `ti` procedure to
handle the case of `Fun f (Check s) xs e`, and thus allow
for **checking type specifications**?

**HINT:** You may want to *factor* out steps 2-5 in
the `ti` definition for `Lam xs body` --- i.e. the
code that checks the `body` has type `tOut` when
`xs` have type `tXs` --- into a separate function
to implement the `ti` cases for

* `Fun f (Infer) xs e`
* `Fun f (Check) xs e`

This is a bit tricky, and so am leaving it as **Extra Extra Credit**.
