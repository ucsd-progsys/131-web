---
title: Data Representation
date: 2016-10-17
headerImg: cobra.jpg
---

Next, lets add support for

* **Multiple datatypes** (`number` and `boolean`)
* **Calling** external functions

In the process of doing so, we will learn about

* **Tagged Representations**
* **Calling Conventions**

## Plan

Our plan will be to (start with `boa`) and add the following features:

1. **Representing** boolean values (and numbers)

2. **Arithmetic Operations**

3. **Arithmetic Comparisons**

4. **Dynamic Checking** (to ensure operators are well behaved)

## 1. Representation

### Motivation: Why booleans?

In the year 2018, its a bit silly to use

+ `0` for `false` and
+ non-zero for `true`.

But really, `boolean` is a stepping stone to other data

+ Pointers,
+ Tuples,
+ Structures,
+ Closures.

### The Key Issue

How to _distinguish_ numbers from booleans?

* Need to store some _extra_ information to mark values as `number` or `bool`.

### Option 1: Use _Two_ Words

First word is `0` means `bool`, is `1` means `number`, `2` means pointer etc.


|    Value |   Representation (HEX)        |
|---------:|------------------------------:|
|       `3`|    `[0x000000000][0x00000003]`|
|       `5`|    `[0x000000000][0x00000005]`|
|      `12`|    `[0x000000000][0x0000000c]`|
|      `42`|    `[0x000000000][0x0000002a]`|
|   `false`|    `[0x000000001][0x00000000]`|
|    `true`|    `[0x000000001][0x00000001]`|

Pros

* Can have _lots_ of different types, but

Cons

* Takes up _double_ memory,
* Operators `+`, `-` do _two_ memory reads `[eax]`, `[eax - 4]`.

In short, rather wasteful. Don't need _so many_ types.

### Option 2: Use a _Tag Bit_

Can distinguish _two_ types with a _single bit_.

*Least Significant Bit* (LSB) is

* `0` for `number`
* `1` for `boolean`

(Hmm, why not `0` for `boolean` and `1` for `number`?)

### Tag Bit: Numbers

So `number` is the binary representation shifted left by 1 bit

* Lowest bit is always `0`
* Remaining bits are number's binary representation

For example,

|    Value |               Representation (Binary) |
|---------:|--------------------------------------:|
|       `3`| `[0b00000000000000000000000000000110]`|
|       `5`| `[0b00000000000000000000000000001010]`|
|      `12`| `[0b00000000000000000000000000011000]`|
|      `42`| `[0b00000000000000000000000001010100]`|

Or in HEX,

|    Value |   Representation (HEX) |
|----------|-------------------------
|       `3`|          `[0x00000006]`|
|       `5`|          `[0x0000000a]`|
|      `12`|          `[0x00000018]`|
|      `42`|          `[0x00000054]`|

### Tag Bit: Booleans

*Most Significant Bit* (MSB) is

* `1` for `true`
* `0` for `false`

For example

|    Value |               Representation (Binary) |
|---------:|--------------------------------------:|
|    `true`| `[0b10000000000000000000000000000001]`|
|   `false`| `[0b00000000000000000000000000000001]`|

Or, in HEX

|    Value |   Representation (HEX) |
|---------:|-----------------------:|
|    `true`|          `[0x80000001]`|
|   `false`|          `[0x00000001]`|


### Types

Lets extend our source types with `boolean` constants

```haskell  
data Expr a
  = ...
  | Boolean Bool a
```  

Correspondingly, we extend our assembly `Arg` (values) with

```haskell
data Arg
  = ...
  | HexConst  Int
```

So, our examples become:

|           Value |   Representation (HEX) |
|----------------:|-----------------------:|
|  `Boolean False`|   `HexConst 0x00000001`|
|   `Boolean True`|   `HexConst 0x80000001`|
|       `Number 3`|   `HexConst 0x00000006`|
|       `Number 5`|   `HexConst 0x0000000a`|
|      `Number 12`|   `HexConst 0x0000000c`|
|      `Number 42`|   `HexConst 0x0000002a`|


### Transforms

Next, lets update our implementation

![Compiler Pipeline](/static/img/compiler-pipeline-representation.png)

The `parse`, `anf` and `tag` stages are straightforward.

Lets focus on the `compile` function.

#### A TypeClass for Representing Constants

Its convenient to introduce a type class describing Haskell types that can
be _represented_ as x86 arguments:

```haskell
class Repr a where
  repr :: a -> Arg
```

We can now define instances for `Int` and `Bool` as:

```haskell
instance Repr Int where
  repr n = Const (Data.Bits.shift n 1) -- left-shift `n` by 1

instance Repr Bool where
  repr False = HexConst 0x00000001
  repr True  = HexConst 0x80000001
```  

#### Immediate Values to Arguments

`Boolean b` is an _immediate_ value (like `Number n`).

Lets extend `immArg` that tranforms an immediate expression to an x86 argument.

```haskell
immArg :: Env -> ImmTag -> Arg
immArg (Var    x _)  = ...
immArg (Number n _)  = repr n
immArg (Boolean b _) = repr b
```

#### Compiling Constants

Finally, we can easily update the `compile` function as:

```haskell
compileEnv :: Env -> AnfTagE -> Asm
compileEnv _ e@(Number _ _)  = [IMov (Reg EAX) (immArg env e)]
compileEnv _ e@(Boolean _ _) = [IMov (Reg EAX) (immArg env e)]
```

(The other cases remain unchanged.)

Lets run some tests to double check.

### QUIZ

What is the result of:

```haskell
ghci> exec "15"
```

* **A** Error
* **B** `0`
* **C** `15`
* **D** `30`


### Output Representation


Say what?! Ah. Need to update our run-time printer in `main.c`

```c
void print(int val){
  if (val == CONST_TRUE)
    printf("true");
  else if (val == CONST_FALSE)
    printf("false");
  else // should be a number!
    printf("%d", d >> 1);  // shift right to remove tag bit.
}
```

and now we get:

```haskell
ghci> exec "15"
15
```

Can you think of some other tests we should write?

### QUIZ

What is the result of

```haskell
ghci> exec "let x = 15 in x"
```

* **A** Error
* **B** `0`
* **C** `15`
* **D** `30`


### QUIZ

What is the result of

```haskell
ghci> exec "if 3: 12 else: 49"
```

* **A** Error
* **B** `0`
* **C** `12`
* **D** `49`

## 2. Arithmetic Operations

Constants like `2`, `29`, `false` are only useful if we can perform
computations with them.

First lets see what happens with our arithmetic operators.

### QUIZ: Addition

What will be the result of:

```haskell
ghci> exec "12 + 4"
```

1. Does not compile
2. Run-time error (e.g. segmentation fault)
3. `16`
4. `32`
5. `0`

### Shifted Representation and Addition

We are _representing_ a number `n` by **shifting it left by 1**

> `n` has the machine representation `2*n`

Thus, our _source values_ have the following _representations:

|    Source Value |         Representation (DEC) |
|----------------:|-----------------------------:|
|              `3`|                          `6` |
|              `5`|                         `10` |
|      `3 + 5 = 8`|                `6 + 10 = 16` |
|        `n1 + n2`|  `2*n1 + 2*n2 = 2*(n1 + n2)` |

That is, _addition_ (and similarly, _subtraction_)
works _as is_ with the shifted representation.

### QUIZ: Multiplication

What will be the result (using our code so far) of:

```haskell
ghci> exec "12 * 4"
```

1. Does not compile
2. Run-time error (e.g. segmentation fault)
3. `24`
4. `48`
5. `96`

Hmm. What happened?

### Shifted Representation and Multiplication

We are _representing_ a number `n` by **shifting it left by 1**

> `n` has the machine representation `2*n`

Thus, our _source values_ have the following _representations:

|    Source Value |         Representation (DEC) |
|----------------:|-----------------------------:|
|              `3`|                          `6` |
|              `5`|                         `10` |
|     `3 * 5 = 15`|                `6 * 10 = 60` |
|        `n1 * n2`|  `2*n1 * 2*n2 = 4*(n1 + n2)` |

Thus, multiplication ends up accumulating the factor of 2.
* Result is _two times_ the desired one.

### Strategy

Thus, our strategy for compiling arithmetic operations is simply:

* Addition and Subtraction "just work" as before, as shifting "cancels out",
* Multiplication result must be "adjusted" by dividing-by-two
  - i.e. **right shifting by 1**

### Types

The _source_ language does not change at all, for the `Asm`
lets add a "right shift" instruction (`shr`):

```haskell
data Instruction
  = ...
  | IShr    Arg   Arg
```

### Transforms

We need only modify `compileEnv` to account for the "fixing up"

```haskell
compileEnv :: Env -> AnfTagE -> [Instruction]
compileEnv env (Prim2 o v1 v2 _) = compilePrim2 env o v1 v2
```

where the helper `compilePrim2` works for `Prim2` (binary) operators
and _immediate arguments_:

```haskell
compilePrim2 :: Env -> Prim2 -> ImmE -> ImmE -> [Instruction]
compilePrim2 env Plus v1 v2   = [ IMov (Reg EAX) (immArg env v1)
                                , IAdd (Reg EAX) (immArg env v2)
                                ]
compilePrim2 env Minus v1 v2  = [ IMov (Reg EAX) (immArg env v1)
                                , ISub (Reg EAX) (immArg env v2)
                                ]
compilePrim2 env Times v1 v2  = [ IMov (Reg EAX) (immArg env v1)
                                , IMul (Reg EAX) (immArg env v2)
                                , IShr (Reg EAX) (Const 1)
                                ]
```

### Tests

Lets take it out for a drive.

```haskell
ghci> exec "2 * (-1)"
2147483644
```

Whoa?!

Well, its easy to figure out if you look at
the generated assembly:

```nasm
mov eax, 4
imul eax, -2
shr eax, 1
ret
```

The trouble is that the **negative** result of the multiplication is
saved in **twos-complement** format, and when we shift that right by
one bit, we get the wierd value (**does not "divide by two"**)

| Decimal      | Hexadecimal |                             Binary   |
|-------------:|------------:|-------------------------------------:|
|         `-8` | `0xFFFFFFF8`| `0b11111111111111111111111111111000` |
| `2147483644` | `0x7FFFFFFC`| `0b01111111111111111111111111111100` |

**Solution: Signed/Arithmetic Shift**

The instruction `sar`
[shift arithmetic right](https://en.wikibooks.org/wiki/X86_Assembly/Shift_and_Rotate#Arithmetic_Shift_Instructions)
does what we want, namely:

* preserves the sign-bit when shifting
* i.e. doesn't introduce a `0` by default

### Transforms Revisited

Lets add `sar` to our target:

```haskell
data Instruction
  = ...
  | ISar Arg Arg
```

and use it to fix the post-multiplication adjustment

* i.e. use `ISar` instead of `IShr`

```haskell
compilePrim2 env Times v1 v2  = [ IMov (Reg EAX) (immArg env v1)
                                , IMul (Reg EAX) (immArg env v2)
                                , ISar (Reg EAX) (Const 1)
                                ]
```

After which all is well:

```haskell
ghci> exec "2 * (-1)"
-2
```

## 3. Arithmetic Comparisons

Next, lets try to implement comparisons:

```haskell
ghci> exec "1 < 2"
...
boa: lib/Language/Boa/Compiler.hs:(104,1)-(106,43): Non-exhaustive patterns in function compilePrim2
```

Oops. Need to implement it first!

Many ways to do this:

* branches `jne, jl, jg` or
* bit-twiddling.

### Comparisons via Bit-Twiddling

**Key idea:**

> A *negative* number's **most significant bit** is `1`

To implement `arg1 < arg2`, compute `arg1 - arg2`
* When result is negative, MSB is `1`, ensure `eax` set to `0x80000001`
* When result is non-negative, MSB is `0`, ensure `eax` set to `0x00000001`

1. Can **extract msb** by bitwise `and` with `0x80000000`.
2. Can **set tag bit** by bitwise ` or` with `0x00000001`

So compilation strategy is:

```nasm
mov eax, arg1
sub eax, arg2
and eax, 0x80000000   ; mask out "sign" bit (msb)
or  eax, 0x00000001   ; set tag bit to bool
```

### Comparisons: Implementation

Lets go and extend:

1. The `Instruction` type

```haskell
data Instruction
  = ...
  | IAnd    Arg   Arg
  | IOr     Arg   Arg
```

2. The `instrAsm` converter

```haskell
instrAsm :: Instruction -> Text
instrAsm (IAnd a1 a2) = ...
instrAsm (IOr  a1 a2) = ...
```

3. The actual `compilePrim2` function   



### Exercise: Comparisons via Bit-Twiddling

* Can compute `arg1 > arg2`  by computing `arg2 < arg1`.
* Can compute `arg1 != arg2` by computing `arg1 < arg2 || arg2 < arg1`
* Can compute `arg1 = arg2`  by computing `! (arg1 != arg2)`

For the above, can you figure out how to implement:

1. Boolean `!`  ?
2. Boolean `||` ?
3. Boolean `&&` ?

You may find [these instructions useful](https://en.wikibooks.org/wiki/X86_Assembly/Logic)

## 4. Dynamic Checking

We've added support for `Number` and `Boolean` but we have no way to ensure
that we don't write gibberish programs like:

```haskell
2 + true
```

or

```haskell
7 < false
```

In fact, lets try to see what happens with our code on the above:

```haskell
ghci> exec "2 + true"
```

Oops.

### Checking Tags at Run-Time

Later, we will look into adding a _static_ type system that will reject
such meaningless programs at _compile_ time. For now, lets see how to
extend the compilation to _abort execution_ when the wrong _types of operands_
are found when the code is _executing_.

The following table lists the types of operands that _should be_ allowed
for each primitive operation.


| Operation |           Op-1 |           Op-2 |
|----------:|---------------:|---------------:|
| `+`       |           `int`|           `int`|
| `-`       |           `int`|           `int`|
| `*`       |           `int`|           `int`|
| `<`       |           `int`|           `int`|
| `>`       |           `int`|           `int`|
| `&&`      |          `bool`|          `bool`|
| `||`      |          `bool`|          `bool`|
| `!`       |          `bool`|                |
| `if`      |          `bool`|                |
| `=`       | `int` or `bool`| `int` or `bool`|


### Strategy

Lets check that the data in `eax` is an `int`
* Suffices to check that the LSB is `0`
* If not, jump to special `error_non_int` label

For example, to check if `arg` is a `Number`

```nasm
mov eax, arg
mov ebx, eax              ; copy into ebx register
and ebx, 0x00000001       ; extract lsb
cmp ebx, 0                ; check if lsb equals 0
jne error_non_number      
...
```

at `error_non_number` we can call into a `C` function:

```
error_non_number:
  push eax                ; pass erroneous value
  push 0                  ; pass error code
  call error              ; call run-time "error" function
```

Finally, the `error` function is part of the _run-time_ and looks like:

```c
void error(int code, int v){
   if (code == 0) {
     fprintf(stderr, "Error: expected a number but got %#010x\n", v);
   }
   else if (code == 1) {
     // print out message for errorcode 1 ...
   }
   else if (code == 2) {
     // print out message for errorcode 2 ...
   } ...
   exit(1);
 }
```

### Strategy By Example

Lets implement the above in a simple file `tests/output/int-check.s`

```nasm
section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  mov eax, 1                ; not a valid number
  mov ebx, eax              ; copy into ebx register
  and ebx, 0x00000001       ; extract lsb
  cmp ebx, 0                ; check if lsb equals 0
  jne error_non_number      
error_non_number:
  push eax
  push 0
  call error
```

Alas

```bash
make tests/output/int-check.result
... segmentation fault ...
```

What happened ?

### Managing the Call Stack

To properly call into C functions (like `error`), we must play by the rules of
the [C calling convention](http://www.cs.virginia.edu/~evans/cs216/guides/x86.html#calling)

![Stack Frames](/static/img/stack-frames.png)

1. The _local variables_ of an (executing) function are saved in its _stack frame_.
2. The _start_ of the stack frame is saved in register `ebp`,
3. The _start_ of the _next_ frame is saved in register `esp`.

### Calling Convention

We must **preserve the above invariant** as follows:

#### In the Callee

At the **start** of the function

```nasm
push ebp          ; save (previous, caller's) ebp on stack
mov ebp, esp      ; make current esp the new ebp
sub esp, 4*N      ; "allocate space" for N local variables
```

At the **end** of the function

```nasm
mov esp, ebp      ; restore value of esp to that just before call
                  ; now, value at [esp] is caller's (saved) ebp
pop ebp           ; so: restore caller's ebp from stack [esp]
ret               ; return to caller
```

#### In the Caller

To call a function `target` that takes `N` parameters:

```nasm
push arg_N        ; push last arg first ...
...
push arg_2        ; then the second ...
push arg_1        ; finally the first
call target       ; make the call (which puts return addr on stack)
add esp, 4*N      ; now we are back: "clear" args by adding 4*numArgs
```

**NOTE:** If you are compiling on MacOS, you must respect the
[16-Byte Stack Alignment Invariant](http://www.fabiensanglard.net/macosxassembly/index.php)


### Fixed Strategy By Example

Lets implement the above in a simple file `tests/output/int-check.s`

TODO

```nasm
section .text
extern error
extern print
global our_code_starts_here
our_code_starts_here:
  push ebp
  mov ebp, esp
  sub esp, 0                ; 0 local variables here
  mov eax, 1                ; not a valid number
  mov ebx, eax              ; copy into ebx register
  and ebx, 0x00000001       ; extract lsb
  cmp ebx, 0                ; check if lsb equals 0
  jne error_non_number      
  mov esp, ebp
  pop ebp  
  ret
error_non_number:
  push eax
  push 0
  call error
```

Aha, now the above works!

```bash
make tests/output/int-check.result
... expected number but got ...
```

**Q:** What NEW thing does our compiler need to compute?

**Hint:** Why do we `sub esp, 0` above?


### Types

Lets implement the above strategy.

To do so, we need a new data type for run-time types:

```haskell
data Ty = TNumber | TBoolean
```

a new `Label` for the error

```haskell
data Label
  = ...
  | TypeError Ty        -- Type Error Labels
  | Builtin   Text      -- Functions implemented in C
```

and thats it.

### Transforms

The compiler must generate code to:

1. Perform dynamic type checks,
2. Exit by calling `error` if a failure occurs,
3. Manage the stack per the convention above.

#### 1. Type Assertions

The key step in the implementation is to write a function

```haskell
assertType :: Env -> IExp -> Ty -> [Instruction]
assertType env v ty
  = [ IMov (Reg EAX) (immArg env v)
    , IMov (Reg EBX) (Reg EAX)
    , IAnd (Reg EBX) (HexConst 0x00000001)
    , ICmp (Reg EBX) (typeTag  ty)
    , IJne (TypeError ty)
    ]
```

where `typeTag` is:

```haskell
typeTag :: Ty -> Arg
typeTag TNumber  = HexConst 0x00000000
typeTag TBoolean = HexConst 0x00000001  
```

You can now splice `assertType` prior to doing the actual
computations, e.g.

```haskell
compilePrim2 :: Env -> Prim2 -> ImmE -> ImmE -> [Instruction]
compilePrim2 env Plus v1 v2   = assertType env v1 TNumber
                             ++ assertType env v2 TNumber  
                             ++ [ IMov (Reg EAX) (immArg env v1)
                                , IAdd (Reg EAX) (immArg env v2)
                                ]
```

#### 2. Errors

We must also add code _at_ the `TypeError TNumber`
and `TypeError TBoolean` labels.

```haskell
errorHandler :: Ty -> Asm
errorHandler t =
  [ ILabel   (TypeError t)        -- the expected-number error
  ,   IPush  (Reg EAX)            -- push the second "value" param first,
  ,   IPush  (ecode t)            -- then the first  "code" param,
  ,   ICall  (Builtin "error")    -- call the run-time's "error" function.  
  ]

ecode :: Ty -> Arg   
ecode TNumber  = Const 0
ecode TBoolean = Const 1
```

#### 4. Stack Management

**Local Variables**

First, [note that](#calling-convention) the local variables live at offsets
from `ebp`, so lets update

```haskell
immArg :: Env -> ImmTag -> Arg
immArg _   (Number n _) = Const n
immArg env (Var    x _) = RegOffset EBP i
  where
    i                   = fromMaybe err (lookup x env)
    err                 = error (printf "Error: Variable '%s' is unbound" x)
```

**Maintaining `esp` and `ebp`**

We need to make sure that _all_ our code respects
[the C calling convention.](#calling-convention).

To do so, just _wrap_ the generated code, with
instructions to save and restore `ebp` and `esp`

```haskell
compileBody :: AnfTagE -> Asm
compileBody e = entryCode e
             ++ compileEnv emptyEnv e
             ++ exitCode e

entryCode :: AnfTagE -> Asm
entryCode e = [ IPush (Reg EBP)
              , IMov  (Reg EBP) (Reg ESP)
              , ISub  (Reg ESP) (Const 4 * n)
              ]
  where
    n       = countVars e

exitCode :: AnfTagE -> Asm
exitCode = [ IMove (Reg ESP) (Reg EBP)
           , IPop  (Reg EBP)
           , IRet
           ]
```

**Q:** But how shall we compute `countVars`?

Here's a shady kludge:

```haskell
countVars :: AnfTagE -> Int
countVars = 100
```

Obviously a sleazy hack (_why?_), but lets use it
to _test everything else_; then we can fix it.

## 5. Computing the Size of the Stack

Ok, now that everything (else) seems to work, lets work out:

```haskell
countVars :: AnfTagE -> Int
```

Finding the _exact_ answer is **undecidable** in general (CSE 105),
i.e. is _impossible_ to compute.

However, it is easy to find an _overapproximate_ heuristic, i.e.

* a value guaranteed to be _larger_ than the than the max size,

* and which is reasonable in practice.

As usual, lets see if we can work out a heuristic by example.

### QUIZ

How many stack slots/vars are needed for the following program?

```haskell
1 + 2
```

1. `0`
2. `1`
3. `2`



### QUIZ

How many stack slots/vars are needed for the following program?

```haskell
let x = 1
  , y = 2
  , z = 3
in
  x + y + z
```

1. `0`
2. `1`
3. `2`
4. `3`
5. `4`

### QUIZ

How many stack slots/vars are needed for the following program?

```haskell
if true:
  let x = 1
    , y = 2
    , z = 3
  in
    x + y + z
else:
  0
```

1. `0`
2. `1`
3. `2`
4. `3`
5. `4`

### QUIZ

How many stack slots/vars are needed for the following program?

```haskell
let x =
  let y =
    let z = 3  
    in z + 1
  in y + 1
in x + 1
```

1. `0`
2. `1`
3. `2`
4. `3`
5. `4`

### Strategy

Let `countVars e` be:

* The _maximum_ number of let-binds in scope at any point _inside_ `e`, i.e.

* The _maximum_ size of the `Env` when compiling `e`

Lets work it out on a case-by-case basis:

* **Immediate values** like `Number` or `Var`
  * are compiled _without pushing_ anything onto the `Env`
  * i.e. `countVars` = 0

* **Binary Operations** like `Prim2 o v1 v2` take immediate values,
  * are compiled _without pushing_ anything onto the `Env`
  * i.e. `countVars` = 0

* **Branches** like `If v e1 e2` can go either way
  * can't tell at compile-time
  * i.e. worst-case is larger of `countVars e1` and `countVars e2`

* **Let-bindings** like `Let x e1 e2` require
  * evaluating `e1` and
  * _pushing_ the result onto the stack and then evaluating `e2`
  * i.e. larger of `countVars e1` and `1 + countVars e2`


### Implementation

We can implement the above a simple recursive function:

```haskell
countVars :: AnfTagE -> Int  
countVars (If v e1 e2)  = max (countVars e1) (countVars e2)
countVars (Let x e1 e2) = max (countVars e1) (1 + countVars e2)
countVars _             = 0
```


### Naive Heuristic is Naive

The above method is quite simplistic. For example, consider the
expression:

```haskell
let x = 1
  , y = 2
  , z = 3
in
    0
```

`countVars` would tell us that we need to allocate `3` stack spaces
but clearly _none_ of the variables are actually used.

Will revisit this problem later, when looking at optimizations.

## Recap

We just saw how to add support for

* **Multiple datatypes** (`number` and `boolean`)
* **Calling** external functions

and in doing so, learned about

* **Tagged Representations**
* **Calling Conventions**

To get some practice, in your assignment, you will add:

1. Dynamic Checks for Arithmetic Overflows (see the `jo` and `jno` operations)
2. A Primitive `print` operation implemented by a function in the `c` run-time.

And next, we'll see how to easily add **user-defined functions**.
