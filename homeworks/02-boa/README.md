# Boa

![A boa](https://upload.wikimedia.org/wikipedia/commons/9/90/Boa_constrictor%2C_Va%C5%88kovka%2C_Brno_%282%29.jpg)

In this assignment you'll implement a small language called Boa, which has

**B**-inary **O**-perators **A**-nd conditionals. You'll use an A-Normal Form
conversion to make binary operator compilation easy, and compile if via `jmp`
instructions.


## The Boa Language

As usual, there are a few pieces that go into defining a language for us to
compile.

1. A description of the **concrete syntax** – the text the programmer writes

2. A description of the **abstract syntax** – how to express what the
   programmer wrote in a data structure our compiler uses.

3. A description of the **semantics** — or **behavior** —of the abstract
  syntax, so our compiler knows what the code it generates should _evaluate_.

### Concrete Syntax

The concrete syntax of Boa is:

```
<expr> :=
  | let <bindings> in <expr>
  | if <expr>: <expr> else: <expr>
  | <binop-expr>

<binop-expr> :=
  | <number>
  | <identifier>
  | add1(<expr>)
  | sub1(<expr>)
  | <expr> + <expr>
  | <expr> - <expr>
  | <expr> * <expr>
  | ( <expr> )

<bindings> :=
  | <identifier> = <expr>
  | <identifier> = <expr>, <bindings>
}
```

As in `adder`, a `let` expression can have one _or more_ bindings.


### Abstract Syntax

The abstract syntax is defined by the data type `Expr`:

```haskell
data Expr a
  = Number  !Integer                       a
  | Id      !Id                            a
  | Prim1   !Prim1    !(Expr a)            a
  | Prim2   !Prim2    !(Expr a)  !(Expr a) a
  | Let     !(Bind a) !(Expr a)  !(Expr a) a
  | If      !(Expr a) !(Expr a)  !(Expr a) a

data Bind a
  = Bind !Id a
```



#### ANF Expressions

An **immediate** expression is a
* constant `Number n`, or
* identifier `Id x`.

```haskell
{-@ measure isImm @-}
isImm :: Expr a -> Bool
isImm (Number  _ _) = True
isImm (Id      _ _) = True
isImm _             = False

{-@ type ImmExpr a = {v:Expr a | isImm v} @-}
```

An **A-Normal Form (ANF)** expression is one where every argument
for a `Prim1` or `Prim2` or conditional (in an `If`) is **immediate**

```haskell
{-@ measure isAnf @-}
isAnf :: Expr a -> Bool
isAnf (Number  _ _)    = True
isAnf (Id      _ _)    = True
isAnf (Prim1 _ e _)    = isImm e
isAnf (Prim2 _ e e' _) = isImm e && isImm e'
isAnf (If c t e _)     = isImm c && isAnf t && isAnf e
isAnf (Let _ e e' _)   = isAnf e && isAnf e'

{-@ type AnfExpr a = {v:Expr a| isAnf v} @-}
```

#### Bare Expressions

We use the `a` parameter to capture different kinds of meta-data about the
expressions. For example, the parser returns values where each sub-expression
is labeled by its `SourceSpan` (to enable proper error messages).

```haskell
type Bare     = Expr SourceSpan
```

#### Tagged Expressions

Finally, the compilation (code generation) works with AST
nodes labeled by `Tag`. We will ensure each sub-expression
has a _distinct_ tag; consequently we can use the tags
to generate _distinct_ control-flow labels for branches.

```haskell
type Tag   = (SourceSpan, Int)
type AExp  = AnfExpr Tag
type IExp  = ImmExpr Tag
```

### Semantics

Numbers, unary operators, let-bindings, and ids have the same semantics as
before.  Binary operator expressions evaluate their arguments and combine them
based on the operator.  

`If` expressions behave similarly to if statements in `C`:
1. The conditional (first part) is evaluated.  
2. If the conditional is `0`, the **else** branch is evaluated;
3. Otherwise (if the conditional is non-zero), the **then** branch is evaluated.

### Examples

For example,

```haskell
sub1(5)               -- ===> 4

if 5 - 5: 6 else: 8   -- ===> 8

let x = 10
  , y = 9
in
   if (x - y) * 2:
     x
   else:
     y                -- ===> 10
```

## Implementing Boa

### New Assembly

#### Labels

We have introduced a new type to represent control-flow labels:

```haskell
data Label
  = BranchTrue Tag
  | BranchDone Tag
```  

here `Tag` is an alias for `Int`; the above lets us define "true" and "done"
labels for each `Tag`.

The new `Instruction` are:

- `IMul Arg Arg` — Multiply the left argument by the right argument, and
  store in the left argument (typically the left argument is `eax` for us)

- `ICmp Arg Arg` — Compares the two arguments for equality.  Set the
  _condition code_ in the machine to track if the arguments were equal, or if
  the left was greater than or less than the right.  This information is used
  by `jne` and other conditional jump commands.

  Example: `cmp [esp-4], 0`

  Example: `cmp eax, [esp-8]`

- `ILabel Label` — Create a location in the code that can be jumped to
  with `jmp`, `jne`, and other jump commands.

- `IJne Label` — If the _condition code_ says that the last comparison
  (`cmp`) was given equal arguments, do nothing.  If it says that the last
  comparison was _not_ equal, immediately start executing instructions from
  the given `Label` (by changing the program counter).

- `IJe Label` — Like `IJne` but with the jump/no jump cases reversed

- `IJmp Label` — Unconditionally start executing instructions from the
  given label (by changing the program counter)

To flex your muscles regarding the above, fill in the implementation of:

```haskell
-- lib/Language/Boa/Asm.hs
instrAsm (ICmp a1 a2)   = error "TBD:instrAsm:cmp"
instrAsm (ILabel l)     = error "TBD:instrAsm:label"
instrAsm (IJe  l)       = error "TBD:instrAsm:je"
instrAsm (IJne  l)      = error "TBD:instrAsm:jne"
instrAsm (IJmp l)       = error "TBD:instrAsm:jmp"
```

As usual, full summaries of the instructions we use are at [this assembly
guide](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html).

#### Combining `cmp` and Jumps for If

When compiling an if expression, we need to execute exactly _one_ of the
branches (and not accidentally evaluate both!).  A typical structure for doing
this is to have two labels: one for the _then_ case and one for the _end_ of the
if expression.  So the compiled shape may look like:

```
  cmp eax, 0    ; check if eax is equal to 0
  jne label_true
  ; commands for "else" branch go here
  jmp label_done
label_true:
  ; commands for "then" branch go here
label_done:
```

Note that if we did _not_ put `jmp label_done`
after the commands for the _else_ branch, control
would continue and evaluate the _then_ branch as well.  
So we need to make sure we skip over the else branch
by unconditionally jumping to `label_done`.

We can't simply use the same label names over and over.  The assembler will get confused if we have multiple nested `if` expressions, because it won't know _which_ `label_done` to `jmp` to, for example.  
So we need some way of generating new names that we know
won't conflict.

Thus, in your code, you will use

* `BranchTrue i` and
* `Branchdone i`

where `i` will be derived from the meta-data
tag on the `If` expression to generate suitable
_distinct_ labels for each branch.

Specifically, your task will be to fill in the implementations of:

```haskell
-- lib/Language/Boa/Compiler.hs

immArg env e@(Id x _)   = error "TBD:immArg:Id"

compilePrim1 :: Tag -> Env -> Prim1 -> IExp -> [Instruction]
compilePrim1 l env Add1 v = error "TBD:compilePrim1:Add1"
compilePrim1 l env Sub1 v = error "TBD:compilePrim1:Sub1"

compilePrim2 :: Tag -> Env -> Prim2 -> IExp -> IExp -> [Instruction]
compilePrim2 l env Plus  v1 v2 = error "TBD:compilePrim2:Plus"
compilePrim2 l env Minus v1 v2 = error "TBD:compilePrim2:Minus"
compilePrim2 l env Times v1 v2 = error "TBD:compilePrim2:Times"
```

### Implementing ANF

Aside from conditionals, the other major thing you need to do in the
implementation of `boa` is add an implementation of ANF to convert the
user-facing syntax to the ANF syntax the compiler uses.

Study the cases that are filled in to figure out how the other cases
should go.

You can also see this [detailed write up as guide][lh-blog-anf]

Specifically, your task is to fill in the code for:

```haskell
-- Normalizer.hs
anf i (Let x e b l)     = error "TBD:anf:let"
anf i (Prim2 o e1 e2 l) = error "TBD:anf:prim2"

imm i (Number n l)      = error "TBD:imm:Number"
imm i (Id x l)          = error "TBD:imm:Id"
imm i (Prim2 o e1 e2 l) = error "TBD:imm:prim2"
```

### A Note on Scope

For this assignment, you can assume that all variables have different names.
That means in particular you don't need to worry about nested instances of
variables with the same name, duplicates within a list, etc.  The combination
of these with `anf` causes some complications that go beyond our goals for
this assignment.

### Testing Functions

Finally, add new tests by filling new tests in `tests/Test.hs`.
Specifically, you should add new entries in

* `anfTests` to test your ANF conversion; this takes string representations
  of an `Expr` and an `AnfExpr` and checks that the `anf` function converts
  the former to the latter. You can use this to test your ANF directly, to make sure it's producing the constrained expression you expect, before you try compiling it.

* `compilerTests` to test the compiler, both for outputs and error
  messages as before (in `adder`). You can "test" your `compile` function _without_ implementing _anf_ by using the commented out `compiler` definition (that just omits the call to `anormal`.)
  This is useful if you want to test binary operators and branches
  before you are confident that your ANF transformation works.

### Recommended TODO List

Here's an order in which you could consider
tackling the implementation:

1. Write some tests for the input and output of
   `anf` for nested `Prim1` expressions to understand
   what the ANF transformation looks like on those
   examples.

2. Fill in the `immArg` case and `compilePrim1`
   in `Compiler.hs`. This will let you run things
   like `sub1(sub1(5))` and similarly nested expressions.

3. Finish the `If` case for `compileEnv` (with
   immediate conditions) so you can run simple programs
   with `if`.  This includes the pretty-printing for the
   comparison and jump instructions, etc.

4. Write some tests for the input and output of
   performing `anf` on `EIf` expressions, again
   to get a feel for what the transformation
   looks like.

5. Work through both the `anf` implementation and
   the compiler implementation `compilePrim2`.  
   Write tests as you go.

6. Work through both the `anf` implementation and
   the compiler implementation of `ELet`.  
   Write tests as you go.

## Submission

TODO


[lh-blog-anf]: http://goto.ucsd.edu/~rjhala/liquid/haskell/blog/blog/2016/09/05/normal-forms.lhs/
