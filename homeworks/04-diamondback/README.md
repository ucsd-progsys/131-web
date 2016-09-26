# Diamondback

![A diamondback](https://upload.wikimedia.org/wikipedia/commons/d/d4/Crotalus_ruber_02.jpg)

-------

In this assignment, you'll implement a compiler for a small language with
functions declarations and function calls, conforming to the C stack layout.
You'll also add some static well-formedness checks to the compiler.

This is the best I could do:

- **D**ouble-word
- **I**ntel **A**rchitecture
- **MO**stly dynamic
- **N**ested-expression
- (**D**iamondback supports recursion)
- **B**oolean-tagged
- **A**NF-transformed
- **C**ompiler,
- o**K**?

## The Diamondback Language

As usual, we have concrete and abstract syntaxes, along with a specification
of semantics.

### Concrete Syntax

The major addition to Diamondback are _function declarations_.  Our programs
are now a sequence of zero or more function declarations, followed by a single
_main expression_.

```
<program> :=
  | <decls> <expr>
  | <expr>

<decls> :=
  | <decl>
  | <decl> <decls>

<decl> :=
  | def <identifier>(<ids>): <expr>
  | def <identifier>(): <expr>

<ids> :=
  | <identifier>
  | <identifier> , <ids>

<expr> :=
  | let <bindings> in <expr>
  | if <expr>: <expr> else: <expr>
  | <binop-expr>

<binop-expr> :=
  | <identifier>
  | <number>
  | true
  | false
  | add1(<expr>)
  | sub1(<expr>)
  | isnum(<expr>)
  | isbool(<expr>)
  | print(<expr>)
  | <identifier>(<exprs>)
  | <identifier>()
  | <expr> + <expr>
  | <expr> - <expr>
  | <expr> * <expr>
  | <expr> < <expr>
  | <expr> > <expr>
  | <expr> == <expr>
  | ( <expr> )

<exprs> :=
  | <expr>
  | <expr> , <exprs>

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
```

The other addition is **function applications** or **function calls**,
which are written `<identifier>(<exprs>)`, for example `f(1, 2, 3)`.  

### Abstract Syntax

As usual, we have a user-facing syntax and a compiler-facing syntax.

```haskell
-- lib/Language/Diamondback/Types.hs

-- | A Program is a list of declarations and "main" Expr
data Program a = Prog
  { pDecls :: [Decl a]      -- ^ function declarations
  , pBody  :: !(Expr a)     -- ^ "main" expression
  }

-- | Decl are function definitions
data Decl a = Decl
  { fName  :: !(Bind a)     -- ^ name of function
  , fArgs  :: [Bind a]      -- ^ names of parameters
  , fBody  :: !(Expr a)     -- ^ "body"/returned expression
  , fLabel :: a             -- ^ metadata
  }

-- | Expr are single expressions
data Expr a
  = Number  !Integer                       a  -- ^ integer constant
  | Boolean !Bool                          a  -- ^ boolean constant
  | Id      !Id                            a  -- ^ variable
  | Prim1   !Prim1    !(Expr a)            a  -- ^ unary prim-op
  | Prim2   !Prim2    !(Expr a)  !(Expr a) a  -- ^ binary prim-op
  | If      !(Expr a) !(Expr a)  !(Expr a) a  -- ^ conditional
  | Let     !(Bind a) !(Expr a)  !(Expr a) a  -- ^ let-binder
  | App     !Id       [Expr a]             a  -- ^ function call

-- | `Prim1` are unary operations
data Prim1
  = Add1
  | Sub1
  | Print
  | IsNum
  | IsBool

-- | `Prim2` are binary operations
data Prim2
  = Plus
  | Minus
  | Times
  | Less
  | Greater
  | Equal
```

As before we use `AnfExpr` and `ImmExpr` to describe ANF and immediate expressions. We use, `AnfDecl` and `AnfProgram` for declarations and programs that have been converted to ANF.

### Semantics

There are several distinguishing features of `diamondback`

+ **Function Applications** A function application should give
  the answer we'd get if we followed the rules for substituting
  argument values for parameter names. So, for example:

```python
def f(x, y):
  x + y

f(1, 2)
```

Should produce 3.

Your compiler should use the rules for C stacks discussed
in class and at [this assembly guide](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html)
to implement this behavior.

There are a number of **new errors** that can occur now that
we have function declarations and calls.  Your implementation
should catch all of these cases **statically**; i.e. _before_
the program runs:

- A function application with the **wrong number of arguments**
  should signal an error containing the string `"arity"`
- A function application of a **non-existent function**
  should signal an error containing the string `"not defined"`
- An identifier **without a corresponding binding location**
  should report an error containing the string `"unbound"`
- A let binding that **redefines a variable already in scope**
  should report an error containing the string `"shadow binding"`
- A function declaration with **duplicate names in the argument list**
  should report an error containing the string `"duplicate parameter"`
- **Multiple function definitions** with the same name,
  should result in an error containing the string `"duplicate function"`
- A **numeric constant is too large** (as discussed in class),
  should result in an error containing the string `"too large"`

**Static Error Checking**  
These errors should stop the program from compiling,
_not_ happen at runtime.  You can continue to assume
that all identifiers within a function body have different
names, which is a requirement for ANF (we could implement
another pass to rename variables, but we won't do that here).  
See the notes on `well_formed` below for details on how to
implement these static checks.

### Implementation

You shouldn't need any new assembly instructions to
tackle this implementation.  You're free to add your
own new instructions.

There are a few new pieces to the implementation:

1. Static Error Checking `lib/Language/Diamondback/Checker.hs`
2. ANF Conversion `lib/Language/Diamondback/Normalizer.hs`
3. Assembly Generation `lib/Language/Diamondback/Compiler.hs`

#### 1. Static Error Checking

A set of `wellFormed` functions, defined in `Checker.hs`,
which are called prior to performing ANF and are used
to report the errors above.  

```haskell
wellFormed  :: BareProgram -> [UserError]
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
```

As before, `BareX` refers to the version of `X` (i.e. `Program`, `Decl` or `Expr`) where the meta-data field is simply `SourceSpan`.


These functions all return a `[UserError]` -- i.e. a list of
`UserError` that represent **all the errors** in the respective
program, declaration or expression. So a program like

```python
def f(x, x):
  y

f(1)
```

would report **three errors**, one for `y` being unbound, one for duplicated `x` parameters, and one for the arity mismatch on the
call to `f`. To ensure that the error-messages are sensible, feel
free to use the supplied constructors -- e.g. `errUnboundVar`, `errDupParam` and `errCallArity`. Your task is to ensure that you
invoke the constructors with the correct parameters, found while
traversing the body of the program, declaration or expression, respectively.

These errors **can be reported in any order**;
in general (and in grading), it's easy to test
for one at a time. Reporting many makes using
the `main` of the compiler much more pleasant,
and is a nice view into the kinds of compiler
ergonomics we should expect from a modern compiler.


#### 2. ANF Conversion

You will have to extend the ANF conversion to account for function
calls, by filling in the definitions for:

```haskell
anf i (App f es l)      = error "TBD:anf:App"

imm i (App f es l)      = error "TBD:imm:App"
```

which convert a function call expression into **ANF** and **immediate** form, respectively.

#### 3. Compiling Definitions and Calls

Your third major task is to implement the compilation
of programs. When you are done, you should be generating
assembly that looks like:

```nasm
  ;; extern and global stuff
fun_decl1:
  ;; code for fun_decl1, including stack management
fun_decl2:
  ;; code for fun_decl2, including stack management
...
our_code_starts_here:
  ;; main entrypoint, as before, with stack management
internal_error_non_number:
  ;; errors, as before
...
```

To do so, complete the definitions of the functions below.

First, the `APgm`, `ADcl` and `AExp` types  
respectively contain ANF- and tagged- versions of
the top-level program, declaration and expressions.
Fill in code to generate `[Instruction]` for each of the above.

```haskell
-- lib/Language/Diamondback/Compiler.hs

compile :: APgm -> [Instruction]
compile (Prog ds e) = error "TBD:compile"

compileDecl :: ADcl -> [Instruction]
compileDecl (Decl f xs e l) = error "TBD:compileDecl"

compileEnv :: Env -> AExp -> [Instruction]
compileEnv env e = error "TBD:compileEnv"
```

Each function's `body` expression is compiled using
`funInstrs n body` which returns the instructions of `body`
wrapped with code that sets up the stack (by allocating
space for n local vars) and restores the callees stack
prior to return.

```haskell
-- lib/Language/Diamondback/Compiler.hs

funInstrs :: Int -> [Instruction] -> [Instruction]
funInstrs n instrs
  = funEntry n
 ++ instrs
 ++ funExit
 ++ [IRet]
```

You will need to fill in the implementation of `funEntry`
and `funExit` which respectively generate the instructions
for setting up stack for `n` local vars and cleaning up
stack prior to returning.

```haskell
-- lib/Language/Diamondback/Compiler.hs

funEntry :: Int -> [Instruction]
funEntry n = error "TBD:funEntry"

funExit :: [Instruction]
funExit = error "TBD:funExit"
```

Finally, fill in the definition of

```haskell
-- lib/Language/Diamondback/Asm.hs

dynError   :: DynError -> [Instruction]
dynError e = error "TBD:dynError"
```

to contain the labels and code for handling the
three different kinds of run-time errors, namely
type-errors and arithmetic overflow.


### Testing

TODO

There is one new testing function provided, `tvg`.  
This works just like `t`, except it runs `valgrind`
on the executable it creates, and checks whether or
not it reports errors.  If `valgrind` does report
memory errors, the test fails and the errors are
reported.  The test succeeds if there are no memory
errors and the answer is correct.

You can test the well-formedness errors using the
error tester as usual.

### Handin

TODO
