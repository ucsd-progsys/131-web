---
title: First Class Functions
date: 2016-11-14
headerImg: fer-de-lance.jpg
---


## Functions as Values

We have functions, but they are *second-class* entities
in our languages: they don't have the same *abilities*
as other values.

For example, the following is rejected:

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

with a multiple error messages:

```
Errors found!
tests/input/hof.diamond:(2:3)-(4:1): Function 'it' is not defined

         2|    it(5)
               ^^

tests/input/hof.diamond:7:3-7: Unbound variable 'incr'

         7|  f(incr)
               ^^^^
```

This is because the `Env` only holds

- parameters, and
- let-bound variables

and **not** function definitions.

But for the many reasons we saw in CSE 130 -- we *want* to treat functions
like values. For example, if you run the above in Python you get:

```python
>>> def f(it): return it(5)
>>> def incr(x): return x + 1
>>> f(incr)
6
```

## Flashback: How do we _compile_ `incr`?

We compile each function down into a sequence of instructions
corresponding to its body.

```python
def incr(x):
  x + 1

incr(5)
```

becomes:

```nasm
label_def_incr_start:
  push ebp                      # setup stack frame
  mov ebp, esp

  mov eax, [ebp + 8]            # grab param
  mov ebx, 2                    # incr by 1
  add eax, ebx

  mov esp, ebp                  # undo stack frame
  pop ebp
  ret                           # buh-bye

our_code_starts_here:         
  push ebp
  mov ebp, esp

  push DWORD 10                 # push arg '5'
  call label_def_incr_start     # call function
  add esp, 4                    # pop  arg from stack

  mov esp, ebp
  pop  ebp
  ret
```

## What is the _value_ of a function?

So now, lets take a step back. Suppose we want to compile

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

## Attempt 1: What is the value of the parameter `it` ?

**IDEA:** Use the **label** where `incr` lives!

```nasm
label_def_f_start:
  push ebp
  mov  ebp, esp

  mov eax, [ebp + 8]            # grab function-address
  push DWORD 10                 # push arg '5'
  call eax                      # call function!
  add esp, 4                    # pop arg from stack

  mov esp, ebp
  pop ebp
  ret
```

## How to pass the value of the parameter ?

So now the `main` expression

```
f(incr)
```

can be compiled to:

```nasm
our_code_starts_here:         
  push ebp
  mov ebp, esp

  push ?1         # push arg
  call ?2         # call function
  add esp, 4      # pop  arg

  mov esp, ebp
  pop  ebp
  ret
```

**QUIZ:** What are suitable terms for `?1` and `?2` ?

|        | `?1`                   |   `?2`                 |
|-------:|:-----------------------|:-----------------------|
| **A**  | `label_def_incr_start` | `labal_def_f_start`    |
| **B**  | `label_def_f_start`    | `labal_def_incr_start` |
| **C**  | `label_def_f_start`    | `labal_def_f_start`    |
| **D**  | `label_def_incr_start` | `labal_def_incr_start` |


## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

## Yay, that was easy! How should the following behave?

```python
def f(it):
  it(5)

def add(x, y):
  x + y

f(incr)
```

Lets cheat and see what Python does:

```python
>>> def f(it): return it(5)
>>> def add(x,y): return x + y
>>> f(add)
Traceback (most recent call last):
  File "<stdin>", line 1, in <module>
  File "<stdin>", line 1, in f
TypeError: add() takes exactly 2 arguments (1 given)
```

## Problem: Ensure Valid Number of Arguments?

How to make sure

* `f(incr)` **succeeds**, but
* `f(add)`  **fails**

With **proper run-time error**?

1. **Where** does the run-time check happen?
2. **What** information is needed for the check?

**Key:** Need to _also_ store the function's **arity**

- The **number of arguments** required by the function


## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`


## Attempt 2: What is the value of the parameter `it` ?

**IDEA:** Use a **tuple of** `(arity, label)`

We can now compile a call

```
  e(x1,...,xn)
```

via the following strategy:

1. **Evaluate** the tuple `e`
2. **Check** that `e[0]` is equal to `n` (else arity mismatch error)
3. **Call** the function at `e[1]`

### Example

Lets see how we would compile this

```python
def f(it):
  it(5)

def incr(x):
  x + 1

f(incr)
```

We need to map **the variable**

  `incr`

to **the tuple**

  `(1, label_def_incr_start)`

But **where** will we store this information?

## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

## Attempt 3: Lambda Terms

So far, we could only define functions at **top-level**

* First-class functions are like _any_ other expression,

* Can define a function, wherever you have any other expression.

| **Language** | **Syntax**                    |
|:-------------|:------------------------------|
| Haskell      | `\(x1,...,xn) -> e`           |
| Ocaml        | `fun (x1,...,xn) -> e`        |
| JS           | `(x1,...,xn) => { return e }` |
| C++          | `[&](x1,...,xn){ return e }`  |


### Example: Lambda Terms

We can now replace `def` as:

```python
let f    = (lambda (it): it(5))
  , incr = (lambda  (x): x + 1)
in
  f(incr)
```

### Implementation

As always, to the details! Lets figure out:

**Representation**

1. How to store function-tuples

**Types:**

1. Remove `Def`
2. Add `lambda` to `Expr`

**Transforms**

1. Update `tag` and `ANF`
2. Update `checker`
3. Update `compile`

### Implementation: Representation

Represent ``lambda-tuples'' or ``function-tuples'' via a special tag:

| Type       |   LSB |
|-----------:|------:|
| `number`   |  xx0  |
| `boolean`  |  111  |
| `pointer`  |  001  |
| `function` |  101  |

In our code:

```haskell
data Ty = ... | TClosure

typeTag :: Ty -> Arg
typeTag TTuple    = HexConst 0x00000001
typeTag TClosure  = HexConst 0x00000005

typeMask :: Ty -> Arg
typeMask TTuple   = HexConst 0x00000007
typeMask TClosure = HexConst 0x00000007
```

So, **Function Values** represented just like a tuples

* padding, `ESI` etc.
* but with tag `101`.

Crucially, we can **get** `0`-th, or `1`-st elements from tuple.

**Question:** Why not use _plain tuples_?

### Implementation: Types

First, lets look at the  new `Expr` type

* No more `Def`

```haskell
data Expr a
  = ...
  | Lam               [Bind a]   !(Expr a) a      -- fun. definitions
  | App     !(Expr a) [Expr a]             a      -- fun. calls
```

So we represent a **function-definition** as:

```haskell
Lam [x1,...,xn] e
```

and a **function call** as:

```haskell
App e [e1,...,en]
```

### Transforms: Tag

This is pretty straight forward (do it yourself)

### Transforms: ANF

QUIZ:

```haskell
  (App e es)
```

Does `e` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above


### Transforms: ANF

QUIZ:

```haskell
  (App e es)
```

Do `es` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above


### Transforms: ANF

The `App` case, fun + args should be **immediate**

* **Need the values to push on stack and make the call happen!**

Just like function calls (in `diamondback`), except

* Must also handle the **callee-expression** (named `e` below)

```haskell
anf i (App e es)   = (i', stitch bs (App v vs))
  where
    (i', bs, v:vs) = imms i (e:es)
```


### Transforms: ANF

QUIZ:

```haskell
  (Lam xs e)
```

Does `e` need to be

* **A** Immediate  
* **B** ANF
* **C** None of the above


### Transforms: ANF

The `Lam` case, the body will be **executed** (when called)

* So we just need to make sure its in ANF (like all the code!)

```haskell
anf i (Lam xs e) = (i', Lam xs e')
  where
    (i', e')     = anf i e
```

### Transforms: Checker

We just have `Expr` (no `Def`) so there is a single function:

```haskell
wellFormed :: BareExpr -> [UserError]
wellFormed = go emptyEnv
  where
    gos                       = concatMap . go
    go _    (Boolean {})      = ...
    go _    (Number  n     l) = largeNumberErrors      n l
    go vEnv (Id      x     l) = unboundVarErrors  vEnv x l
    go vEnv (Prim1 _ e     _) = ...
    go vEnv (Prim2 _ e1 e2 _) = ...
    go vEnv (If   e1 e2 e3 _) = ...
    go vEnv (Let x e1 e2   _) = ... ++ go vEnv e1 ++ go (addEnv x vEnv) e2
    go vEnv (Tuple es      _) = ...
    go vEnv (GetItem e1 e2 _) = ...
    go vEnv (App e es      _) = ?1
    go vEnv (Lam xs e      _) = ?2 ++ go ?3 e
```

* How shall we implement `?1` ?

* How shall we implement `?2` ?

* How shall we implement `?3` ?


### Transforms: Compiler

Finally, lets see how to convert `Expr` into `Asm`, two separate cases:

* `Lam` : definitions

* `App` : calls

### Transforms: Compiler: `Lam`

```haskell
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env (Lam xs e l)
  = IJmp   end              -- Why?
  : ILabel start            -- Function start
  : compileDecl l xs e      -- Function code (like Decl)
 ++ ILabel end              -- Function end
  : lamTuple arity start    -- Compile fun-tuple into EAX
  where
    arity = length xs
    start = LamStart l
    end   = LamEnd   l
```

**QUESTION:** Why do we start with the `IJmp`?

```haskell
lamTuple :: Int -> Label -> [Instruction]
lamTuple arity start
  =  tupleAlloc  2                           -- alloc tuple size = 2  
  ++ tupleWrites [ repr arity                -- fill arity
                 , CodePtr start ]           -- fill code-ptr
  ++ [ IOr  (Reg EAX) (typeTag TClosure) ]   -- set the tag bits
```

### Transforms: Compiler: `App`

**Recall! IDEA:** Use a **tuple of** `(arity, label)`

We can now compile a call

```
  e(x1,...,xn)
```

via the following strategy:

1. **Evaluate** the tuple `e`
2. **Check** that `e[0]` is equal to `n` (else arity mismatch error)
3. **Call** the function at `e[1]`

```haskell
compileEnv env (App vE vXs)
  = assertType     env vE TClosure                    -- check vE is a function
 ++ assertArity    env vE (length vXs)                -- check vE arity
 ++ tupleReadRaw   (immArg env vE) (repr (1 :: Int))  -- load vE[1] into EAX
 ++ [IPush (param env vX) | vX <- reverse vXs]        -- push args
 ++ [IPush (param env vE)]                            -- push closure-ptr
 ++ [ICall (Reg EAX)]                                 -- call EAX
 ++ [IAdd  (Reg ESP) (4 * (n + 1)]                    -- pop  args
```

## A Problem: Scope

Consider the following program:

```haskell
let one = 1
  , f   = (lambda (it): it(5))
  , inc = (lambda (n): n + one)
in
  f(inc)
```

## A Problem Magnified: Dynamically Created Functions

Will it work? How about this variant:

```haskell
let add    = (lambda (n): (lambda (m): n + m))
  , f      = (lambda (it): it(5))
  , plus1  = add(1)
  , plus10 = add(10)
in
  (f(plus1), f(plus10))
```

* `add(1)` should evaluate to a **function-that-adds-1**
* `add(10)` should evaluate to a **function-that-adds-10**

Yet, its **the same code**

- same arity
- same start-label

**Problem:** How can we represent _different behaviors?_

## Free and Bound Variables

A variable `x` is **bound** inside an expression `e` if

- `x` is a let-bound variable inside `e`.
- `x` is a formal parameter in `e`, OR

A variable `x` is **free** inside an expression `e` if

- `x` is **not bound** inside `e`

For example consider the expression `e` :

```python
lambda (m):
  let t = m in
    n + t
```

- `m`, `t` are  **bound** inside `e`, but,
- `n` is **free** inside `e`

## Computing Free Variables

Lets write a function to **compute** the set of free variables.

**Question** Why *Set* ?

```haskell
freeVars :: Expr -> [Id]
freeVars e = S.toList (go e)
  where
    go :: Expr -> S.Set Id
    go (Id x)          = S.singleton x
    go (Number _)      = S.empty
    go (Boolean _)     = S.empty
    go (If e e1 e2)    = S.unions (map go [e1, e2, e3])
    go (App e es)      = S.unions (map go (e:es))
    go (Let x e1 e2)   = S.union (go e1) (S.delete x (go e2))
    go (Lam xs e)      = S.difference (go e) (S.fromList xs)=
```

lambda (x1,x2,x3): e

let x = y + 10 in
  x + z

A. {x}
B. {}
C. { gobble_gobble }

**TODO-IN-CLASS**

## Free Variables and Lambdas

**Free Variables** of a `lambda`

- Those whose values come from *outside*
- Should use *the same* values whenever we "call" the `lambda`.

For example:

```haskell
let add    = (lambda (n): (lambda (m): n + m))
  , f      = (lambda (it): it(5))
  , plus1  = add(1)
  , plus10 = add(10)
in
  (f(plus1), f(plus10), plus10(20))
```

should evaluate to `(6, 15, 30)`

- `plus1` be like `lambda (m): 1  + m`
- `plus1` be like `lambda (m): 10 + m`

## Achieving Closure

(Recall from CSE 130)

**Key Idea:**  Each function value must **store its free variables**

represent `plus1` as:

```
(arity, code-label, [n := 1])
```

represent `plus10` as:

```
(arity, code-label, [n := 10])
```

Same code, but different free variables.

## Strategy Progression

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

    - **Problem:** How to store local variables?

4. **Function Value** `(Arity, Start-Label, Free_1, ... , Free_N)`

    - **Ta Da!**

## Closures: Strategy

What if we have *multiple* free variables?

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z))
  , plus10 = foo(4, 6)
  , plus20 = foo(7, 13)
in
  (plus10(0), plus20(100))
```

represent `plus10` as:

```
(arity, code-label, [x := 4], [y := 6])
```

represent `plus20` as:

```
(arity, code-label, [x := 7], [y := 13])
```

### Example

Lets see how to evaluate

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z))
  , plus10 = foo(4, 6)
in
  plus10(0)
```

TODO: PIC

### Example

Lets see how to evaluate

```python
let foo    = (lambda (x, y):
                (lambda (z): x + y + z))
  , plus10 = foo(4, 6)
  , f      = (lambda (it): it(5))
in
  f(plus10)
```

TODO: PIC

### Implementation

**Representation**

1. How to store closures

**Types:**

- Same as before

**Transforms**

1. Update `tag` and `ANF`
    - as before

2. Update `checker`       

3. Update `compile`

### Representation

We can represent a **closure** as a tuple of

```
(arity, code-ptr, free-var-1, ... free-var-N)
```

which means, following the convention for tuples, as:

```
-------------------------------------------------------------------------
| N + 2 | arity | code-ptr | var1 | var2 | ... | varN | (maybe padding) |
-------------------------------------------------------------------------
```

Where each cell represents 32-bits / 4-bytes / 1-word.

**Note:** (As with all tuples) the first word contains the #elements of the tuple.

* Which, in this case, it is `N + 2`


### Transforms: Checker

What environment should we use to check a `Lam` **body** ?

```haskell
wellFormed :: BareExpr -> [UserError]
wellFormed = go emptyEnv
  where
    ...  
    go vEnv (Lam xs e _) = errDupParams xs
                        ++ go ?vEnv e

addsEnv :: Env -> [BareBind] -> Env
addsEnv env xs = foldr addEnv env xs
```

**QUIZ** How shall we implement `?vEnv` ?

**A.** `addsEnv vEnv     []`

**B.** `addsEnv vEnv     xs`

**C.** `addsEnv emptyEnv xs`

### Transforms: Compile

**Question** How does the called function **know** the values of free vars?

- Needs to **restore them** from closure tuple

- Needs to **access** the closure tuple!

... But how shall we give the called function **access** to the tuple?

**By passing the tuple as an _extra parameter_**


### Transforms: Compile

**Calls** `App`

1. **Push** parameters
2. **Push** closure-pointer-parameter
3. **Call** code-label
4. **Pop**  params + pointer

**Definitions** `Lam`

1. **Compute** *free-vars* `x1`,...,`xn`
2. **Generate** code-block
  - **Restore** free vars from closure-pointer-parameter
  - **Execute** function body (as before)
3. **Allocate** tuple `(arity, code-label, x1, ... , xn)`

### Transforms: Compile Calls

1. **Push** parameters
2. **Push** closure-pointer
3. **Call** code-label
4. **Pop**  params + pointer

### Transforms: Compile Definitions

1. **Compute** *free-vars* `y1`,...,`yn`
2. **Generate** code-block
  - **Restore** free vars from closure-pointer-parameter
  - **Execute** function body (as before)
3. **Allocate** tuple `(arity, code-label, y1, ... , yn)`

```haskell
compileEnv :: Env -> AExp -> [Instruction]
compileEnv env (Lam xs e l)
  = IJmp   end                       -- Why?
  : ILabel start                     -- Function start
  : lambdaBody ys xs e               -- Function code (like Decl)
 ++ ILabel end                       -- Function end
  : lamTuple arity start env ys      -- Compile closure-tuple into EAX
  where
    ys    = freeVars (Lam xs e l)
    arity = length xs
    start = LamStart l
    end   = LamEnd   l
```

### Creating Closure Tuples

To create the actual closure-tuple we need

* the **free-variables** `ys`
* the `env` from which to **values** of the free variables.

```haskell
lamTuple :: Int -> Label -> Env -> [Id] -> [Instruction]
lamTuple arity start env ys
  =  tupleAlloc  (2 + length ys)                    -- alloc tuple 2 + |ys|  
  ++ tupleWrites ( repr arity                       -- fill arity
                 : CodePtr start                    -- fill code-ptr
                 : [immArg env (Id y) | y <- ys] )  -- fill free-vars
  ++ [ IOr  (Reg EAX) (typeTag TClosure) ]          -- set the tag bits
```

### Generating Code Block


```haskell
lambdaBody :: [Id] -> [Id] -> AExp -> [Instruction]
lambdaBody ys xs e = funInstrs maxStack
                        ( restore ys           -- restore free vars from closure-ptr
                       ++ compileEnv env e )   -- exec function-body as before
  where
    maxStack       = envMax env + countVars e  -- max stack size
    env            = fromListEnv bs
    bs             = zip xs  [-2,-3..]         -- put params    into env/stack
                  ++ zip ys  [1..]             -- put free-vars into env/stack
```

To `restore ys` we use the closure-ptr passed
in at `[EBP+8]` --  the special **first** parameter -- to
copy the free-vars `ys` onto the stack.

```haskell
restore :: [Id] -> [Instruction]
restore ys  = concat [ copy i | (y, i) <- zip ys [1..]]
  where
    closPtr = RegOffset 8 EBP
    copy i  = tupleReadRaw closPtr (repr (i+1))  -- copy tuple-fld for y into EAX...
           ++ [ IMov (stackVar i) (Reg EAX) ]    -- ...write EAX into stackVar for y
```

## A Problem: Recursion

Oops, how do we write:

```python
def fac(n):
  if (n > 1):
    n * fac(n-1)
  else:
    1

fac(5)  
```

If we try

```python
let fac = (lambda (n):
             if (n < 1):
               1
             else:
               n * fac(n-1))
in fac(5)  
```

We get a variable unbound error!

```
Errors found!
tests/input/fac-bad.fdl:5:20-23: Unbound variable 'fac'

         5|                 n * fac(n-1))
```

We need to teach our compiler that its ok to use the name `fac` inside the body!

### Solution: Named Functions

We have a new form of **named functions**

- Like Ocaml's `let rec`

Which looks like this:

```python
def fac(n):
  if (n < 1):
    1
  else:
    n * fac(n - 1)
in
  fac(5)
```

### Representing Named Functions

We extend `Expr` to handle such functions as:

```haskell
data Expr a
  = ...
  | Fun     (Bind a)      -- ^ name of function
            [Bind a]      -- ^ list of parameters  
            (Expr a) a    -- ^ body of function
```

Note that we parse the code

```python
def f(x1,...,xn):
  e
in
  e'
```

as the `Expr`  

```haskell
Let f (Fun f [x1,...,xn] e) e'
```

### Compiling Named Functions

Mostly, this is left as an exercise to you.

**Non-Recursive** functions

- i.e. `f` *does not* appear inside `e` in `Fun f xs e`
- Treat `Fun f xs e` as `Lam xs e` ...
- ... Everything should _just work_.

**Recursive**

- i.e. `f` *does* appear inside `e` in `Fun f xs e`
- Can you think of a simple tweak to the `Lam` strategy that works?


## Recap: Functions as Values

We have functions, but they are *second-class* entities
in our languages: they don't have the same *abilities*
as other values.

1. **Representation** = `Start-Label`

    - **Problem:** How to do run-time checks of valid args?

2. **Representation** = `(Arity, Start-Label)`

    - **Problem:** How to map function **names** to tuples?

3. **Lambda Terms** Make functions just another expression!

    - **Problem:** How to store local variables?

4. **Function Value** `(Arity, Start-Label, Free_1, ... , Free_N)`

    - **Ta Da!**


**Next:** Adding **static type inference**

- **Faster!** Gets rid of those annoying (and slow!) run-time checks
- **Safer!** Catches problems at compile-time, when easiest to fix!
