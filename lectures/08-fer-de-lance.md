# Fer-De-Lance: First Class Functions


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

## Strategy Progression [FULL]

1. Representation = Start-Label

    - **Problem:** How to do run-time checks of valid args?

2. Representation = (Start-Label, Arity)

    - **Problem:** How to map function **names** to tuples?

3. Lambda Terms: Make functions just another expression!

    - **Problem:** How to store local variables?

3. Function Value = (Start-Label, Arity, Free-Vars)

    - **Ta Da!**

## Problem: Ensure Valid Number of Arguments?

How to make sure

* `f(incr)` **succeeds**, but
* `f(add)`  **fails**

With **proper run-time error**?

1. **Where** does the run-time check happen?
2. **What** information is needed for the check?

**Key:** Need to _also_ store the function's **arity**

- The **number of arguments** required by the function

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

### Representation

We can represent ``function tuples'' via a special tag:

| Type       |   LSB |
|-----------:|------:|
| `number`   |  xx0  |
| `boolean`  |  111  |
| `pointer`  |  001  |
| `function` |  101  |

So, **Function Values** represented just like a tuples

* padding, `ESI` etc.
* but with tag `101`.

Crucially, we can **get** `0`-th, or `1`-st elements from tuple.

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

## Attempt 3: Lambda Terms

Lets replace `def` with a new `lambda` construct:

```python
let f    = (lambda (it): it(5))
  , incr = (lambda  (x): x + 1)
in
  f(incr)
```

Here, `lambda (x1,...,xn): e` is a **function-expression**
corresponding to a function that takes `n` arguments and
returns the body `e`.

| **Language** | **Syntax**                    |
|:-------------|:------------------------------|
| Haskell      | `\(x1,...,xn) -> e`           |
| Ocaml        | `fun (x1,...,xn) -> e`        |
| JS           | `(x1,...,xn) => { return e }` |
| C++          | `[&](x1,...,xn){ return e }`  |



## Tags

tuples    <address>001
functions <instr-addr>101


### Example: Global Names

Using functions as parameters

```python
def f(it):
  it(5, 6)

def g(a, b):
  a + b

def h(z):
  z + 2

f(g) # ok ==> 11

f(h) # bad
```


### Functions as Expressions

```python
def f(x): x + 2
def g(h): h(5)
g(5)
```

Only define functions at "top-level" not _first class!_
* First-class functions are like _any_ other expression,
* Can define a function, wherever you have any other expression.

Lets define a **anonymous function expression**

```python
lambda (x1,...,xn): e
```

an **nameless** function that takes arguments `x1...xn` and returns `e`.

### Example: Closed Lambda

```python
let f = (lambda (x): x + 2)
  , g = (lambda (h): h(5))
in
    g(5)
```

### Example: Open Lambda

```haskell
let x = 5
  , w = 11
  , y = 13
  , f = lambda (z): x + y + z
in
  f(12)
```

### Example: Currying

```haskell
let gt  = (lambda (m): (lambda (n): n > m))
  , gt5 = gt(5)
  , gt6 = gt(6)
in
  ( gt5(4)  -- ==> true
  , gt5(6)  -- ==> false
  )
```

* `freeVars`
* `lambda` = closure creation
* `apply`  = closure restoration
