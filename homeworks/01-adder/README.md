#Adder

![An adder](https://upload.wikimedia.org/wikipedia/commons/2/28/Loch_Shin_adder.JPG)

In this assignment you'll implement a compiler for a small language called
Adder (because it primarily adds things).

## The Adder Language

In each of the next several assignments, we'll introduce a language that we'll
implement.  We'll start small, and build up features incrementally.  We're
starting with Adder, which has just a few features – defining variables, and
primitive operations on numbers.

There are a few pieces that go into defining a language for us to compile.

- A description of the **concrete syntax** – the text the programmer writes

- A description of the **abstract syntax** – how to express what the
  programmer wrote in a data structure our compiler uses.

- A description of the **semantics** — or **behavior** —of the abstract
  syntax, so our compiler knows what the code it generates should _evaluate_.

### Concrete Syntax

The concrete syntax of Adder is:

```
<expr> :=
  | <number>
  | <identifier>
  | let <bindings> in <expr>
  | add1(<expr>)
  | sub1(<expr>)

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
}
```

Here, a `let` expression can have one _or more_ bindings.


### Abstract Syntax

The abstract syntax of Adder is an OCaml datatype, and corresponds nearly
one-to-one with the concrete syntax.

```haskell
data Prim1 =
  | Add1
  | Sub1

data Expr a
  = Number  !Integer                       a
  | Prim1   !Prim1    !(Expr a)            a
  | Let     !(Bind a) !(Expr a)  !(Expr a) a
  | Id      !Id                            a
```

We introduce a type to represent places where
a variable is **defined** (i.e. **bound**)

```haskell
data Bind a
  = Bind !Id a
    deriving (Show, Functor)
```

**NOTE:** Ignore the `!` in the definitions above,
you can just pretend they are not there at all.
(For the curious: they are called [strictness annotations](https://wiki.haskell.org/Performance/Data_types#Strict_fields))

### Semantics

An Adder program always evaluates to a single integer.  `Number`s evaluate to
themselves (so a program just consisting of `Number(5)` should evaluate to the
integer `5`).  Primitive expressions perform addition or subtraction by one on
their argument.  Let bindings should evaluate all the binding expressions to
values one by one, and after each, store a mapping from the given name to the
corresponding value in both (a) the rest of the bindings, and (b) the body of
the let expression.  Identifiers evaluate to whatever their current stored
value is.  There are several examples further down to make this concrete.

Here are some examples of Adder programs:

| Concrete Syntax | Abstract Syntax | Answer |
------------------|-----------------|---------
| 5               | `Number(5)`     | 5      |
| sub1(add1(sub1(5))) | `Prim1(Sub1, Prim1(Add1, Prim1(Sub1, Number(5))))` | 4 |
| let x = 5 in add1(x) | `Let([("x", Number(5))], Prim1(Add1, Id("x")))` | 6 |
| let x = 5, y = sub1(x) in sub1(y) | `Let([("x", Number(5)), ("y", Prim1(Sub1(Id("x"))))], Prim1(Sub1("y")))` | 3 |

## Implementing a Compiler for Adder

You've been given a starter codebase that has several pieces of
infrastructure:

- `Types.hs` Which contains the key type definitions for source and assembly
  programs,

- `Parse.hs` A parser for Adder, which takes concrete
  syntax (text files) and turns it into `Expr SourceSpan`
  (aka `Bare`) values, via the function:


```haskell
parse :: FilePath -> Text -> Expr SourceSpan
```

You don't need to edit `Parse.hs` (or even understand how it works)
in order to complete this assignment.

- `UX.hs` which has code for reporting errors,

- `Utils.hs` which has miscellaneous helper functions.

**You will only edit**

- `Compiler.hs` which has the main code for converting source expressions
  into assembly, via a function

```haskell
compile :: Expr SourceSpan -> [Instruction]
```

and which uses `parse` to produce assembly code from an input Adder text file
via the function `compiler`.

- `Asm.hs` which has the code for rendering our `Instruction` into raw assembly
text.

- `Test.hs` to add new tests for your code.

### Writing the Compiler

The primary task of writing the Adder compiler is simple to state: take an
instance of the `Expr a` datatype and turn it into a list of assembly
`Instruction`.  The provided compiler skeleton is set up to do just this,
broken up over a few functions.

Your task is to fill in the appropriate implementations in place
of `error "TBD"` in the function:

```haskell
-- in Compiler.hs
compileEnv :: Env -> AExp -> [Instruction]
```

Use only the provided `Instruction` types for this assignment;
we will be gradually expanding this as the quarter progresses.  

The `compileEnv` function has an associated helper that
takes some extra arguments to track the variable environment `Env`
and stack offset.  These will be discussed in more detail in lecture.

The other component you need to implement is:

```haskell
-- in Asm.hs
instrAsm :: Instruction -> Text
regAsm   :: Reg -> Text
argAsm   :: Arg -> Text
```

which renders individual instances of the instruction datatype
into a string representation of the instruction (this is done
for you for `mov` and `ret`). This second step is straightforward,
but forces you to understand the syntax of the assembly code you
are generating.  Most of the compiler concepts happen in the
first step, that of generating assembly instructions from abstract
syntax (i.e. `compileEnv`).  

Do use [this assembly guide](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html)
if you have questions about the concrete syntax (or ask) of an instruction.

### Errors

The compiler should **signal an error** if:

1. There is a binding list containing **two or more bindings with the same name**
2. An **identifier is unbound** (there is no surrounding let binding for it)

At any place in your code, you can signal an error by using

```haskell
panic :: String -> SourceSpan -> a
```

1. The `String` parameter is whatever error message you want,
2. The `SourceSpan` is the source position, which is available
   + from an `AExp` or `Bare` by calling `sourceSpan`, or
   + as the variable `l :: Tag` in `compileEnv`.

```haskell
compileEnv :: Env -> AExp -> [Instruction]
compileEnv _   (Number n l)     = [ IMov (Reg EAX) (repr n) ]
compileEnv env (Prim1 Add1 e l) = error "TBD"
compileEnv env (Prim1 Sub1 e l) = error "TBD"
compileEnv env (Id x l)         = error "TBD"
compileEnv env (Let x e1 e2 l)  = error "TBD"
```

### Testing the Compiler

The file `tests/Test.hs` contains code for testing your compiler.

#### Adding new Tests

To add new tests, fill in new tests as elements of the list in `yourTests`.
Each test is of _one of two forms_:

1. `mkTest FILE File (Right RESULT)` or
2. `mkTest FILE File (Left ERROR)`.

In both cases, the actual test-file should be in `tests/input/FILE.adder`.

* In the first case, `RESULT` is a single string containing the correct output
  (of compiling, linking and running the test).

* In the second case, `ERROR` is a _substring_ of the error message that should
  be produced. This includes problems building at the assembler/linker level,
  as well as any explicit errors thrown by `error` or `panic` calls in the
  compiler itself. You _should_ use this case to explicitly test for the two
  error cases mentioned above.


#### Running a Single Test

For each test, we have the following files:

* `tests/input/FILE.adder`   : The source file  
* `tests/output/FILE.s`      : The generated assembly
* `tests/output/FILE.run`    : The binary executable produced after linking
* `tests/output/FILE.result` : The output of running the binary  
* `tests/output/FILE.log`    : The log of all messages generated during compiling & linking.

Only the first of the above is _hand written_ the rest
are all automatically generated.

**To generate assembly** for an individual test, do:

```bash
make tests/output/five.s
```

**To generate an executable** for an individual test, do

```bash
make tests/output/five.run
```

**To run an executable** for an individual test, invoke the
executable after building it,

```bash
./tests/output/five.run
```

or simply do

```bash
make tests/output/five.result
```

**To debug an assembly file** you can hand-edit (or simply write) a file,
`tests/output/FILE.s` and then _execute_ it by doing

```bash
make tests/output/FILE.result
```

to trigger the build-link-execute from that assembly file.  
This can be helpful if you think you're generating mostly-correct
code, but just want to try a small edit to fix something up.  
It's also helpful if you want to hand-write a small assembly
example: you can create `.s` files from scratch in the `output`
directory to experiment with, if you want to practice with assembly
instructions without the compiler in the way.


### Turnin

TODO
