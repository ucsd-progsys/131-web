---
title: A crash course in Haskell
date: 2016-09-26
headerImg: books.jpg
---

We're going to do this real fast; for 131:

> Haskell = Ocaml + Better syntax

*We assume you are familiar with Ocaml*

So: we'll learn Haskell by comparison.

## Type Ascription

**Ocaml** uses  `:` for type ascription

* `e : t` means `e` has type `t`

```ocaml
(12 : int)
```

vs.

**Haskell** uses `::` for type ascription

* `e :: t` means `e` has type `t`

```Haskell
(12 :: Int)
```




## Function Definitions and Calls

**Ocaml**

```ocaml
(* val incr : int -> int *)
let incr x = x + 1
let stincr = fun x -> x + 1

let eleven = incr (10 + 2)
```

vs

**Haskell**

```haskell
incr :: Int -> Int
incr x = x + 1

eleven = incr 10
```

`let` not needed for top-level binding.

## Pattern Matching

**Ocaml**

```ocaml
(* val listSum : int list -> int list *)
let rec listSum xs = match xs with
  | []       -> 0
  | (x::xs') -> x + listSum xs'
```

vs

**Haskell**

```haskell
listSum :: [Int] -> [Int]
listSum xs = case xs of
               []    -> 0
               x:xs' -> x + listSum xs'
```

or better,

```haskell
listSum :: [Int] -> [Int]
listSum []     = 0
listSum (x:xs) = x + listSum xs
```

Haskell allows **different equations** for different cases.




## Colon vs. Double-Colon

**Ocaml**

* uses `::` for *"cons"*
* uses `:`  for *"has type"*

vs

**Haskell**

* uses `:`   for *"cons"*
* uses `::`  for *"has type"*


A handy table

| Operator | Ocaml       | Haskell     |
|:--------:|:-----------:|:-----------:|
| `::`     | "cons"      | "has type"  |
| `:`      | "has type"  | "cons"      |







## Local Variables

**Ocaml**

```ocaml
(* val filter : ('a -> bool) -> 'a list -> 'a list *)
let filter f xs = match xs with
  | []     -> []
  | x::xs' -> let rest = filter f xs' in
              if f x then x :: rest else rest
```

vs

**Haskell**

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) let rest = filter f xs' in
                if f x then x:rest else rest
```

## QUIZ: Local Variables

```haskell
quiz    = x + y
  where
    x   = 0
    y   = 1
```

What is the value of `quiz`?

A. Syntax error
B. Type Error
C. `0`
D. `1`
E. Other

## QUIZ: Local Variables

```haskell
quiz    = x + y
  where
    x   = 0
    y   = x + 1
```

What is the value of `quiz`?

A. Syntax error
B. Type Error
C. `0`
D. `1`
E. Other

## QUIZ: Local Variables

```haskell
quiz    = x + y
  where
    y   = x + 1
    x   = 0
```

What is the value of `quiz`?

A. Syntax error
B. Type Error
C. `0`
D. `1`
E. Other

## QUIZ: Local Variables

```haskell
quiz    = x + y
  where
    y   = x + 1
    x   = y
```

What is the value of `quiz`?

A. Syntax error
B. Type Error
C. `0`
D. `1`
E. Other


## Local Variables (revisited)

So we can take

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) let rest = filter f xs' in
                if f x then x:rest else rest
```

and write it better as

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x then x:rest else rest
  where
    rest        = filter f xs'
```

**where** lets you pull local variables aside:

* meaning _exactly same as_ `let`, but
* can specify them in _any_ order.

(Seems strange at first, but truly beautiful.)


## Anonymous Functions


**Ocaml**

```ocaml
(* val negate : ('a -> bool) -> 'a -> bool *)
let negate f = fun x -> not (f x)
```

vs

**Haskell**

```haskell
negate :: (a -> Bool) -> a -> Bool
negate f = \x -> not (f x)
```

Very similar: Ocaml's `fun` is replaced with Haskell's `\`

## Tuples and Lists

**Ocaml**

```ocaml
(* val partition: ('a -> bool) -> 'a list -> ('a list * 'a list) *)
let partition f xs = (filter f xs, filter (negate f) xs)
```

(12, "cat")


vs

**Haskell**

```haskell
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition p xs = (filter p xs, filter (negate p) xs)
```

**Note**

+ Haskell uses `(t1, t2)` vs Ocaml's `(t1 * t2)`
+ Haskell uses `[t]`      vs Ocaml's `t list`


## Larger Example

**Ocaml**

```ocaml
(* val sort : 'a list -> 'a list *)
let rec sort xs = match xs with
  | []     -> []
  | (h::t) -> let (ls, rs) = partition (fun x -> x < h) t in
              sort ls @ [h] @ sort rs
```

vs

**Haskell**

```haskell
sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (h:t)  = sort ls ++ [h] ++ sort rs
  where
    (ls,rs) = partition (\x -> x < h) t
```

## QUIZ: List Comprehensions

What is the value of

```haskell
quiz = [0..5]
```

A. Syntax Error
B. Type Error
C. `[0, 5]`
D. `[0, 1, 2, 3, 4]`
E. `[0, 1, 2, 3, 4, 5]`





## QUIZ: List Comprehensions

What is the value of

```haskell
quiz   = [x * 10 | x <- xs]
  where
    xs = [0..5]
```

A. Syntax Error
B. Type Error
C. `[0, 50]`
D. `[0, 10, 20, 30, 40]`
E. `[0, 10, 20, 30, 40, 50]`




## QUIZ: List Comprehensions

What is the value of

```haskell
quiz   = [x * 10 | x <- xs, x < 3]
  where
    xs = [0..5]
```

A. `[]`
B. `[0]`
C. `[0, 10]`
D. `[0, 10, 20]`
E. `[0, 10, 20, 30]`





## QUIZ: List Comprehensions

We can simplify the `sort` using list comprehensions, as in Python:

```haskell
sort []     = []
sort (h:t)  = sort ls ++ [h] ++ sort rs
  where
    ls      = [x | x <- t, x <= h]
    rs      = [x | x <- t,  h < x]
```

## Defining Data

**Ocaml**

```ocaml
type expr
  = Number of float
  | Plus   of expr * expr
  | Minus  of expr * expr
  | Times  of expr * expr
```

vs

**Haskell**

```haskell
data Mond 
  = Number Double
  | Plus   Mond Mond
  | Minus  Mond Mond 
  | Times  Mond Mond 
```

## Constructing Data

**Ocaml**

```ocaml
let ex0 = Number 5.
let ex1 = Plus  (ex0, Number 7.)
let ex2 = Minus (Number 4., Number 2.)
let ex3 = Times (ex1, ex2)
```

vs

**Haskell**

```haskell
ex0 = Number 5
ex1 = Plus  ex0 (Number 7)
ex2 = Minus (Number 4) (Number 2)
ex3 = Times ex1 ex2
```

**Note**

The _tags_ `Plus`, `Number` etc. are (constructor) functions

```haskell
Plus   :: Expr -> Expr -> Expr
Minus  :: Expr -> Expr -> Expr
Times  :: Expr -> Expr -> Expr
```


## QUIZ: Constructor Types

Given

```haskell
data Expr
  = Number Double
  | Plus   Expr Expr
  | Minus  Expr Expr
  | Times  Expr Expr
```

What is the *type of* `Number` ?

A. `Expr`
B. `Double`
C. `Double -> Expr`
D. `Expr -> Double`
E. `Expr -> Expr`


## Destructing (Accessing) Data

**Ocaml**

```ocaml
(* val eval: expr -> float *)
let rec eval e = match e with
  | Number n       -> n
  | Plus  (e1, e2) -> eval e1 +. eval e2
  | Minus (e1, e2) -> eval e1 -. eval e2
  | Times (e1, e2) -> eval e1 *. eval e2
```

vs

**Haskell**

```haskell
eval :: Expr -> Double
eval (Number    n) = n
eval (Plus  e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
```

Oh look, we wrote a _compiler_!

+ What's the _source_ language?
+ What's the _target_ language?

## Recursive Functions

**Ocaml**

```ocaml
let fact n = if n <= 0 then 1 else n * fact (n-1)
```

vs.

**Haskell**

```haskell
fact n = if n <= 0 then 1 else n * fact (n-1)
```



## Printf Debugging

**Very Very Important**

Q: How to **print out** each input-output pair for calls to `fact`?

**Ocaml**

(as in Java, C, Python...), just print it:

```ocaml
let fact n =
  let res = if n <= 0 then 1 else n * fact (n-1)        in
  let _   = Printf.printf "fact n = %d, res = %d\n" n d in
  res
```

vs

**Haskell**

You can't _just_ print stuff (for very good reasons...)

However, you _can_ do this:

```haskell
import Text.Printf (printf)
import Debug.Trace (trace)

-- trace :: String -> a -> a
fact n  = trace msg res
  where
    msg = printf "fact n = %d, res = %d\n" n res
    res = if n <= 0 then 1 else n * fact (n-1)
```

Which pretty much does what you want.

```haskell
*Foo> fact 5
fact n = 0, res = 1

fact n = 1, res = 1

fact n = 2, res = 2

fact n = 3, res = 6

fact n = 4, res = 24

fact n = 5, res = 120

120
```