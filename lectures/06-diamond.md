---
title: Functions
date: 2016-10-26
headerImg: diamond.jpg
---

Next, we'll build **diamondback** which adds support for

* **User-Defined Functions**

In the process of doing so, we will learn abou  t

* **Static Checking**
* **Calling Conventions**
* **Tail Recursion**

## Plan

1. **Defining** Functions
2. **Checking** Functions
3. **Compiling** Functions
4. **Compiling** Tail Calls

## 1. Defining Functions

First, lets add functions to our language.

As always, lets look at some examples.

### Example: Increment

For example, a function that increments its input:

```python
def incr(x):
  x + 1

incr(10)
```

We have a function definition followed by a single "main"
expression, which is evaluated to yield the program's result, which, in this case, is `11`.


### Example: Factorial

Here's a somewhat more interesting example:

```python
def fac(n):
  let t = print(n) in
  if (n < 1):
    1
  else:
    n * fac(n - 1)

fac(5)
```

This program should produce the result

```
5
4
3
2
1
0
120
```

Suppose we modify the above to produce intermediate results:

```python
def fac(n):
  let t   = print(n)
    , res = if (n < 1):
              1
            else:
              n * fac(n - 1)
  in
    print(res)

fac(5)
```

we should now get:

```
5
4
3
2
1
0
1
1
2
6
24
120
120
```

### Example: Mutually Recursive Functions

For this language, **the function definitions are global**
* any function can call any other function.

This lets us write _mutually recursive_ functions like:

```python
def even(n):
  if (n == 0):
    true
  else:
    odd(n - 1)

def odd(n):
  if (n == 0):
    false
  else:
    even(n - 1)

let t0 = print(even(0)),
    t1 = print(even(1)),
    t2 = print(even(2)),
    t3 = print(even(3))
in
    0
```

**QUIZ** What should be the result of executing the above?

1. `false true  false true  0`
2. `true  false true  false 0`
3. `false false false false 0`
4. `true  true  true  true  0`

### Types

Lets add some new types to represent programs.

#### Bindings

Lets create a special type that represents places
where **variables are bound**,

```haskell
data Bind a = Bind Id a
```

A `Bind` is basically just an `Id` _decorated with_
an `a` which will let us save extra _metadata_
like **tags** or **source positions**

* The metadata will make it easy to report errors.

We will use `Bind` at two places:

1. Let-bindings,
2. Function parameters.

It will be helpful to have a function to extract
the `Id` corresponding to a `Bind`

```haskell
bindId :: Bind a -> Id
bindId (Bind x _) = x
```

#### Programs

A **program** is a list of declarations and _main_ expression.

```haskell
data Program a = Prog
  { pDecls :: [Decl a]    -- ^ function declarations
  , pBody  :: !(Expr a)   -- ^ "main" expression
  }
```

#### Declarations

Each **function** lives is its own **declaration**,

```haskell
data Decl a = Decl
  { fName  :: (Bind a)    -- ^ name
  , fArgs  :: [Bind a]    -- ^ parameters
  , fBody  :: (Expr a)    -- ^ body expression
  , fLabel :: a           -- ^ metadata/tag
  }
```

#### Expressions

Finally, lets add  _function application_ (calls) to the source expressions:

```haskell
data Expr a
  = ...
  | Let     (Bind a) (Expr a)  (Expr a) a
  | App     Id       [Expr a]           a
```

An _application_ or _call_ comprises

* an `Id`, the name of the function being called,
* a list of expressions corresponding to the parameters, and
* a metadata/tag value of type `a`.

(**Note:** that we are now using `Bind` instead of plain `Id` at a `Let`.)

#### Examples Revisited

Lets see how the examples above are represented:

```ghc
ghci> parseFile "tests/input/incr.diamond"
Prog {pDecls = [Decl { fName = Bind "incr" ()
                     , fArgs = [Bind "n" ()]
                     , fBody = Prim2 Plus (Id "n" ()) (Number 1 ()) ()
                     , fLabel = ()}
               ]
     , pBody = App "incr" [Number 5 ()] ()
     }

ghci> parseFile "tests/input/fac.diamond"
Prog { pDecls = [ Decl {fName = Bind "fac" ()
                , fArgs = [Bind "n" ()]
                , fBody = Let (Bind "t" ()) (Prim1 Print (Id "n" ()) ())
                          (If (Prim2 Less (Id "n" ()) (Number 1 ()) ())
                             (Number 1 ())
                             (Prim2 Times (Id "n" ())
                                (App "fac" [Prim2 Minus (Id "n" ()) (Number 1 ()) ()] ())
                                ()) ()) ()
                , fLabel = ()}
                ]
     , pBody  = App "fac" [Number 5 ()] ()
     }
```

## 2. Static Checking

Next, we will look at an _increasingly important_ aspect
of compilation, **pointing out bugs in the code at compile time**

Called **Static Checking** because we do this _without_ (i.e. _before_)
compiling and running ("dynamicking") the code.

There is a huge spectrum of checks possible:

* Code Linting [jslint](http://jshint.com/), [hlint](https://hackage.haskell.org/package/hlint)
* Static Typing
* Static Analysis
* Contract Checking
* Dependent or Refinement Typing

Increasingly, _this_ is the most important phase of a compiler,
and modern compiler engineering is built around making these
checks lightning fast. For more, see [this interview of Anders Hejlsberg][hejlsberg-interview]
the architect of the C# and TypeScript compilers.

### Static Well-formedness Checking

We will look at code linting and, later in the quarter, type systems in 131.

For the former, suppose you tried to compile:

```python
def fac(n):
  let t = print(n) in
  if (n < 1):
    1
  else:
    n * fac(m - 1)

fact(5) + fac(3, 4)
```

We would like compilation to fail, not silently, but with useful messages:

```
$ make tests/output/err-fac.result

Errors found!

tests/input/err-fac.diamond:6:13-14: Unbound variable 'm'

         6|      n * fac(m - 1)
                         ^

tests/input/err-fac.diamond:8:1-9: Function 'fact' is not defined

         8|  fact(5) + fac(3, 4)      
             ^^^^^^^^

tests/input/err-fac.diamond:(8:11)-(9:1): Wrong arity of arguments at call of fac

         8|  fact(5) + fac(3, 4)
                       ^^^^^^^^^
```

We get _multiple_ errors:

1. The variable `m` is not defined,
1. The function `fact` is not defined,
2. The call `fac` has the wrong number of arguments.

Next, lets see how to update the architecture of our
compiler to support these and other kinds of errors.

### Types

An _error message_ type:

```haskell
data UserError = Error
  { eMsg  :: !Text
  , eSpan :: !SourceSpan
  }
  deriving (Show, Typeable)
```

We make it an _exception_ (that can be _thrown_):

```haskell
instance Exception [UserError]
```

We can **create** errors with:

```haskell
mkError :: Text -> SourceSpan -> Error
mkError msg l = Error msg l
```

We can **throw** errors with:

```haskell
abort :: UserError -> a
abort e = throw [e]
```

We **display errors** with:

```haskell
renderErrors :: [UserError] -> IO Text
```

which takes something like:

```haskell
Error
 "Unbound variable 'm'"
 { file      = "tests/input/err-fac"
 , startLine = 8
 , startCol  = 1
 , endLine   = 8
 , endCol    = 9
 }
```

and produce a pretty message (that requires reading the source file),

```
tests/input/err-fac.diamond:6:13-14: Unbound variable 'm'

         6|      n * fac(m - 1)
                         ^
```

We can put it all together by

```haskell
main :: IO ()
main = runCompiler `catch` esHandle

esHandle :: [UserError] -> IO ()
esHandle es = renderErrors es >>= hPutStrLn stderr >> exitFailure
```

Which runs the compiler and if any `UserError` are thrown, `catch`-es and
renders the result.

### Transforms

Next, lets insert a `checker` phase into our pipeline:

![Compiler Pipeline with Checking Phase](/static/img/compiler-pipeline-functions-check.png)

In the above, we have defined the types:

```haskell
type BareP   = Program SourceSpan        -- ^ each sub-expression has source position metadata
type AnfP    = Program SourceSpan        -- ^ each function body in ANF
type AnfTagP = Program (SourceSpan, Tag) -- ^ each sub-expression has unique tag
```

### Catching Multiple Errors

To make using a language and compiler pleasant,
we should return _as many errors as possible_ in each run.

* Its rather irritating to get errors one-by-one.

We will implement this by writing the functions

```haskell
wellFormed  :: BareProgram -> [UserError]
```

which will _recursively walk over_ the entire
program, declaration and expression and
return the _list of all errors_.

* If this list is empty, we just return the source unchanged,
* Otherwise, we `throw` the list of found errors (and exit.)

Thus, our `check` function looks like this:

```haskell
check :: BareProgram -> BareProgram
check p = case wellFormed p of
            [] -> p
            es -> throw es
```

### Well-formed Programs, Declarations and Expressions

The bulk of the work is done by:

```haskell
wellFormed :: BareProgram -> [UserError]
wellFormed (Prog ds e)
  =  duplicateFunErrors ds
  ++ concatMap (wellFormedD fEnv) ds
  ++ wellFormedE fEnv emptyEnv e
  where
    fEnv  = fromListEnv [(bindId f, length xs)
                          | Decl f xs _ _ <- ds]
```

This function,

1. **creates** `fEnv`, a map from _function-names_ to the _function-arity_ (number of params),
2. **computes** the errors for each declaration (given functions in `fEnv`),
3. **concatenates** the resulting lists of errors.

### Traversals

Lets look at how we might find three types of errors:

1. "unbound variables"
2. "undefined functions"

(In your assignment, you will look for many more.)

The helper function `wellFormedD` creates an _initial_
variable environment `vEnv` containing the functions
parameters, and uses that (and `fEnv`) to walk over
the body-expressions.

```haskell
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = wellFormedE fEnv vEnv e
  where
    vEnv                         = addsEnv xs emptyEnv
```

The helper function `wellFormedE` starts with the input `vEnv0` (which has just)
the function parameters, and `fEnv` that has the defined functions, and traverses
the expression:

* At each **definition** `Let x e1 e2`, the variable `x`
  is added to the environment used to check `e2`,
* At each **use** `Id x` we check if `x` is in `vEnv`
  and if not, create a suitable `UserError`
* At each **call** `App f es` we check if `f` is in `fEnv`
  and if not, create a suitable `UserError`.

```haskell
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv vEnv0 e      = go vEnv0 e
  where
    gos vEnv es               = concatMap (go vEnv) es
    go _    (Boolean {})      = []
    go _    (Number  n     l) = []
    go vEnv (Id      x     l) = unboundVarErrors vEnv x l
    go vEnv (Prim1 _ e     _) = go  vEnv e
    go vEnv (Prim2 _ e1 e2 _) = gos vEnv [e1, e2]
    go vEnv (If   e1 e2 e3 _) = gos vEnv [e1, e2, e3]
    go vEnv (Let x e1 e2   _) = go vEnv e1
                             ++ go (addEnv x vEnv) e2
    go vEnv (App f es      l) = unboundFunErrors fEnv f l
                             ++ gos vEnv es
```

You should understand the above and be able to easily add extra error checks.

**QUIZ** Which function(s) would we have to modify to
add _large number errors_ (i.e. errors for numeric literals
that may cause overflow)?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

**QUIZ** Which function(s) would we have to modify to
add _variable shadowing errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

**QUIZ** Which function(s) would we have to modify to
add _duplicate parameter errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

**QUIZ** Which function(s) would we have to modify to
add _duplicate function errors_ ?

1. `wellFormed  :: BareProgram -> [UserError]`
2. `wellFormedD :: FunEnv -> BareDecl -> [UserError]`
3. `wellFormedE :: FunEnv -> Env -> Bare -> [UserError]`
4. `1` and `2`
5. `2` and `3`

## 3. Compiling Functions

![Compiler Pipeline for Functions](/static/img/compiler-pipeline-functions.png)

In the above, we have defined the types:

```haskell
type BareP   = Program SourceSpan        -- ^ each sub-expression has source position metadata
type AnfP    = Program SourceSpan        -- ^ each function body in ANF
type AnfTagP = Program (SourceSpan, Tag) -- ^ each sub-expression has unique tag
```
### Tagging

![Compiler Pipeline ANF](/static/img/compiler-pipeline-functions-tag.png)

The `tag` phase simply recursively tags each
function body and the main expression

### ANF Conversion

![Compiler Pipeline ANF](/static/img/compiler-pipeline-functions-anf.png)

* The `normalize` phase (i.e. `anf`) is recursively
  applied to each function body.

* In addition to `Prim2` operands, each call's arguments
  should be transformed into an [immediate expression](04-boa.md/#idea-immediate-expressions)

Generalize the [strategy for _binary_ operators](04-boa.md/#anf-implementation)

* from (`2` arguments) to `n`-arguments.


### Strategy

Now, lets look at _compiling_ function _definitions_ and _calls_.

![Compiler Pipeline with Checking Phase](/static/img/compiler-pipeline-functions-codegen.png)

We need a co-ordinated strategy for _definitions_ and _calls_.

**Definitions**
* Each _definition_ is compiled into a labeled block of `Asm`
* That implements the _body_ of the definitions.
* (But what about the _parameters_)?

**Calls**
* Each _call_ of `f(args)` will execute the block labeled `f`
* (But what about the _parameters_)?

### Strategy: The Stack

![Stack Frames](/static/img/stack-frames.png)

We will use our old friend, _the stack_ to

* pass _parameters_
* have _local variables_ for called functions.

**Calling Convention**

Recall that we are using the `C` calling convention that ensures
the following stack layout:

![Stack Layout](/static/img/stack-layout.png)

### Strategy: Definitions

When the function body starts executing,
the parameters `x1`, `x2`, ... `xn` are at
`[ebp + 4*2]`, `[ebp + 4*3]`, ... `[ebp + 4*(n+1)]`.

1. Ensure that enough stack space is _allocated_ i.e.
   that `esp` and `ebp` are [properly managed](/lectures/05-cobra.md/#managing-the-call-stack)

2. Compile body with _initial_ `Env` mapping parameters
   to `-2`, `-3`,...,`-(n+1)`.

### Strategy: Calls

[As before](/lectures/05-cobra.md/#in-the-caller) we must ensure
that the parameters actually live at the above address.

1. _Before_ the call, `push` the parameter values onto the stack in reverse order,
2. _Call_ the appropriate function (using its label),
3. _After_ the call, _clear_ the stack by incrementing `esp` appropriately.

**NOTE:**

At both _definition_ and _call_, if you are compiling on MacOS,
you need to also respect the [16-Byte Stack Alignment Invariant][mac-os-stack-alignment]

### Types

We already have most of the machinery needed to compile calls.

Lets just add a new kind of `Label` for each user-defined function:

```haskell
data Label
  = ...
  | DefFun Id
```

We will also extend the `Arg` type to include information
about [size directives][evans-x86-guide]

```haskell
data Arg
  = ...
  | Sized Size Arg
```

We will often need to specify that an `Arg` is a _double word_  
(the other possibilities are -- single `word` and `byte`) which
we needn't worry about.

```haskell
data Sized
  = DWordPtr
```


### Implementation

Lets can refactor our `compile` functions into:

```haskell
compileProg ::        AnfTagP -> Asm
compileDecl ::        AnfTagD -> Asm
compileExpr :: Env -> AnfTagE -> Asm
```

that respectively compile `Program`, `Decl` and `Expr`.

In order to **simplify stack managment**
[as in Cobra](/lectures/05-cobra.md/#4-stack-management)
lets have a helper function that compiles the _body_ of
each function:

```haskell
compileBody :: Env -> AnfTagE -> Asm
```

`compileBody env e` will wrap the `Asm` generated by
`compileExpr env e` with the code that manages `esp` and `ebp`.

#### Compiling Programs

To compile a `Program` we compile each `Decl` and the main body expression

```haskell
compileProg (Prog ds e)
  =  compileBody emptyEnv e
  ++ concatMap   compileDecl ds
```

**QUIZ:**

Does it matter whether we put the code for `e` before `ds`?

1. Yes
2. No


**QUIZ:**

Does it matter what order we compile the `ds` ?

1. Yes
2. No

#### Compiling Declarations  

To compile a single `Decl` we

1. Create a block starting with a label for the function's name
   (so we know where to `call`),
2. Invoke `compileBody` to fill in the assembly code for the body,
   using the initial `Env` obtained from the function's formal parameters.

```haskell
compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e _)
  = ILabel (DefFun (bindId f))
  : compileBody (paramsEnv xs) e
```

The initial `Env` is created by `paramsEnv` which returns an `Env`
mapping each [parameter to its stack position](#strategy-definitions)

```haskell
paramsEnv :: [Bind a] -> Env
paramsEnv xs = fromListEnv (zip xids [-2, -3..])
  where
    xids     = map bindId xs
```

(Recall that `bindId` [extracts](#bindings) the `Id` from each `Bind`)

Finally, as in cobra, `compileBody env e` wraps the assmbly for `e`
with the code that manages `esp` and `ebp`.

```haskell
compileBody :: Env -> AnfTagE -> Asm
compileBody env e = entryCode e
                 ++ compileExpr env e
                 ++ exitCode e
                 ++ [IRet]

entryCode :: AnfTagE -> Asm
entryCode e = [ IPush (Reg EBP)
              , IMov  (Reg EBP) (Reg ESP)
              , ISub  (Reg ESP) (Const 4 * n)
              ]
  where
    n       = countVars e

exitCode :: AnfTagE -> Asm
exitCode = [ IMov (Reg ESP) (Reg EBP)
           , IPop (Reg EBP)
           ]
```

#### Compiling Calls

Finally, lets extend code generation to account for calls:

```haskell
compileExpr :: Env -> AnfTagE -> [Instruction]
compileExpr env (App f vs _)
  = call (DefFun f) [param env v | v <- vs]
```

The function `param` converts an **immediate expressions**
(corresponding to function arguments)

```haskell
param :: Env -> ImmE -> Arg
param env v = Sized DWordPtr (immArg env v)
```

The `Sized DWordPtr` specifies that each argument will occupy
a double word (i.e. 4 bytes) on the stack.

**EXERCISE** The hard work in compiling calls is done by:

```haskell
call :: Label -> [Arg] -> [Instruction]
```

which [implements the strategy for calls](#strategy-calls).
Fill in the implementation of `call` yourself. As an example,
of its behavior, consider the (source) program:

```python
def add2(x, y):
  x + y

add2(12, 7)
```

The call `add2(12, 7)` is represented as:

```haskell
App "add2" [Number 12, Number 7]
```

The code for the above call is generated by

```haskell
call (DefFun "add2") [arg 12, arg 7]
```

where `arg` [converts source values into assembly `Arg`](/lectures/05-cobra.md/a-typeclass-for-representing-constants)
which _should_ generate the equivalent of the assembly:

```nasm
  push DWORD 14
  push DWORD 24
  call label_def_add2
  add esp, 8
```

## 4. Compiling Tail Calls

Our language doesn't have _loops_. While recursion is more general,
it is more _expensive_ because it uses up stack space (and requires
all the attendant management overhead). For example (the `python` program):

```python
def sumTo(n):
  r = 0
  i = n
  while (0 <= i):
    r = r + i
    i = i - 1
  return r

sumTo(10000)
```

* Requires a _single_ stack frame
* Can be implemented with 2 registers

But, the "equivalent" `diamond` program

```python
def sumTo(n):
  if (n <= 0):
    0
  else:
    n + sumTo(n - 1)

sumTo(10000)
```

* Requires `10000` stack frames ...
* One for `fac(10000)`, one for `fac(9999)` etc.

### Tail Recursion

Fortunately, we can do much better.

A **tail recursive** function is one where the
recursive call is the _last_ operation done by
the function, i.e. where the value returned by
the function is the _same_ as the value returned
by the recursive call.

We can rewrite `sumTo` using a tail-recursive `loop`
function:

```python
def loop(r, i):
  if (0 <= i):
    let rr = r + i
      , ii = i - 1
    in
      loop(rr, ii)   # tail call
  else:
    r

def sumTo(n):
  loop(0, n)

sumTo(10000)
```

### Visualizing Tail Calls

Lets compare the execution of the two versions of `sumTo`

#### Plain Recursion

```python
sumTo(5)
==> 5 + sumTo(4)
        ^^^^^^^^
==> 5 + [4 + sumTo(3)]
             ^^^^^^^^
==> 5 + [4 + [3 + sumTo(2)]]
                  ^^^^^^^^
==> 5 + [4 + [3 + [2 + sumTo(1)]]]
                       ^^^^^^^^
==> 5 + [4 + [3 + [2 + [1 + sumTo(0)]]]]
                            ^^^^^^^^
==> 5 + [4 + [3 + [2 + [1 + 0]]]]
                        ^^^^^
==> 5 + [4 + [3 + [2 + 1]]]
                   ^^^^^
==> 5 + [4 + [3 + 3]]
              ^^^^^
==> 5 + [4 + 6]
         ^^^^^
==> 5 + 10
    ^^^^^^
==> 15
```

* Each call **pushes a frame** onto the call-stack;
* The results are **popped off** and _added_ to the parameter at that frame.

#### Tail Recursion

```python
sumTo(5)
==> loop(0, 5)
==> loop(5, 4)
==> loop(9, 3)
==> loop(12, 2)
==> loop(14, 1)
==> loop(15, 0)
==> 15
```

* Accumulation happens in the parameter (not with the output),
* Each call returns its result _without further computation_

No need to use call-stack, can make recursive call **in place**.
* Tail recursive calls can be _compiled into loops_!

### Tail Recursion Strategy

Instead of using `call` to make the call, simply:

1. **Move** the _call's_ arguments to the (same) stack position (as current args),
2. **Free** current stack space by resetting `esp` and `ebp` (as just prior to `ret` c.f. `exitCode`),
3. **Jump** to the _start_ of the function.

That is, here's what a _naive_ implementation would look like:

```nasm
push [ebp - 8]        # push ii
push [ebp - 4]        # push rr
call def_loop
```

but a _tail-recursive_ call can instead be compiled as:

```nasm
mov eax , [ebp - 8]  # overwrite i with ii
mov [ebp + 12], eax  
mov eax, [ebp - 4]   # overwrite r with rr
mov [ebp + 8], eax   
mov esp, ebp         # "free" stack frame (as before `ret`)
pop ebp
jmp def_loop         # jump to function start
```

which has the effect of executing `loop` _literally_ as if it were a while-loop!

#### Requirements

To _implement_ the above strategy, we need a way to:

1. **Identify** tail calls in the source `Expr` (AST),
2. **Compile** the tail calls following the above strategy.

### Types

We can do the above in a single step, i.e., we could
identify the tail calls _during_ the code generation,
but its cleaner to separate the steps into:

![Labeling `Expr` with Tail Calls](/static/img/compiler-pipeline-tails.png)

In the above, we have defined the types:

```haskell
type BareP     = Program SourceSpan                 -- ^ each sub-expression has source position metadata
type AnfP      = Program SourceSpan                 -- ^ each function body in ANF
type AnfTagP   = Program (SourceSpan, Tag)          -- ^ each sub-expression has unique tag
type AnfTagTlP = Program ((SourceSpan, Tag), Bool)  -- ^ each call is marked as "tail" or not
```

### Transforms

Thus, to implement tail-call optimization, we need to write _two_ transforms:

**1. To Label** each call with `True` (if it is a _tail call_) or `False` otherwise:

```haskell
tails :: Program a -> Program (a, Bool)
```

**2. To Compile** tail calls, by extending `compileExpr`

#### Labeling Tail Calls

![Which Calls are Tail Calls?](/static/img/tail-rec-code-and-type.png)

The `Expr` in _non tail positions_

* `Prim1`
* `Prim2`
* `Let` ("bound expression")
* `If`  ("condition")

**cannot contain** tail calls; all those values have some further computation
performed on them.

However, the `Expr` in _tail positions_

* `If` ("then" and "else" branch)
* `Let` ("body")

**can contain** tail calls (_unless_ they appear under the first case)

**Algorithm:** Traverse `Expr` using a `Bool`

* Initially `True` but
* Toggled to `False` under _non-tail positions_,
* Used as "tail-label" at each call.

**NOTE:** All non-calls get a default tail-label of `False`.

```haskell
tails :: Expr a -> Expr (a, Bool)
tails = go True                                         -- initially flag is True
  where
    noTail l z             = z (l, False)
    go _ (Number n l)      = noTail l (Number n)        
    go _ (Boolean b l)     = noTail l (Boolean b)
    go _ (Id     x l)      = noTail l (Id x)

    go _ (Prim2 o e1 e2 l) = noTail l (Prim2 o e1' e2')
      where
        [e1', e2']         = go False <$> [e1, e2]      -- "prim-args" is non-tail

    go b (If c e1 e2 l)    = noTail l (If c' e1' e2')
      where
        c'                 = go False c                 -- "cond" is non-tail
        e1'                = go b     e1                -- "then" may be tail
        e2'                = go b     e2                -- "else" may be tail

    go b (Let x e1 e2 l)   = noTail l (Let x e1' e2')  
      where
        e1'                = go False e1                -- "bound-expr" is non-tail
        e2'                = go b     e2                -- "body-expr" may be tail

    go b (App f es l)      = App f es' (l, b)           -- tail-label is current flag
      where
        es'                = go False <$> es            -- "call args" are non-tail
```

**EXERCISE:**
How could we modify the above to _only_ mark **tail-recursive**
calls, i.e. to the _same_ function (whose declaration is being compiled?)


#### Compiling Tail Calls

Finally, to generate code, we need only add a special case to `compileExpr`

```haskell
compileExpr :: Env -> AnfTagTlE -> [Instruction]
compileExpr env (App f vs l)
  | isTail l                 = tailcall (DefFun f) [param env v | v <- vs]
  | otherwise                = call     (DefFun f) [param env v | v <- vs]
```

That is, _if_ the call is _not labeled_ as a tail call,
generate code as before. Otherwise, use `tailcall` which
implements our [tail recursion strategy](#tail-recursion-strategy)

```haskell
tailcall :: Label -> [Arg] -> [Instruction]
tailcall f args
  = moveArgs args   -- overwrite current param stack-slots with call args
 ++ exitCode        -- restore ebp and esp
 ++ [IJmp f]        -- jump to start
```

**EXERCISE**

Does the above strategy work _always_? Can you think of situations where it may
go horribly wrong?

## Recap

We just saw how to add support for **first-class function**

* **Definitions**, and
* **Calls**

and a way in which an important class of

* **Tail Recursive** functions can be compiled as **loops**.

Later, we'll see how to represent **functions as values** using **closures**.


[evans-x86-guide]:        http://www.cs.virginia.edu/~evans/cs216/guides/x86.html
[mac-os-stack-alignment]: http://www.fabiensanglard.net/macosxassembly/index.php
[hejlsberg-interview]:    https://www.infoq.com/news/2016/05/anders-hejlsberg-compiler