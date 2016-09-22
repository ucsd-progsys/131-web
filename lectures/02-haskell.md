# A Crash Course in Haskell

We're going to do this real fast; for 131:

> Haskell = Ocaml + Better syntax + Better tools

*We assume you are familiar with Ocaml*

So: we'll learn Haskell by comparison.

## Type Ascription

Ocaml uses a `:` for type ascription 

* `e : t` means `e` has type `t` 

```ocaml 
(12 : int)
```

Haskell uses `::` for type ascription

* `e :: t` means `e` has type `t` 

```Haskell
(12 :: int)
```




## Function Definitions and Calls

Ocaml

```ocaml
(* val incr : int -> int *)
let incr x = x + 1

let eleven = incr 10
```

vs

Haskell

```haskell
incr :: Int -> Int
incr x = x + 1

eleven = incr 10
```

`let` not needed for top-level binding.

## Pattern Matching

```ocaml
(* val listSum : int list -> int list *)
let rec listSum xs = match xs with
  | []       -> 0
  | (x::xs') -> x + listSum xs'
```

vs


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

Different _equations_ for different cases.

## Colon vs. Double-Colon 

* Ocaml uses `::` for "cons" but Haskell uses `:` for "cons".
* Ocaml uses `:`  for "has type" but Haskell uses `::` for "has type".


| Operator | Ocaml       | Haskell     |
|---------:|------------:|------------:| 
| `::`     | "cons"      | "has type"  |
| `:`      | "has type"  | "cons"      |





## Local Variables

```ocaml
(* val filter : ('a -> bool) -> 'a list -> 'a list *)
let filter f xs = match xs with
  | []     -> []
  | x::xs' -> let rest = filter f xs' in
              if f x then x :: rest else rest
```

vs

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) let rest = filter f xs' in
                if f x then x:rest else rest
```

or, better

```haskell
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x then x:rest else rest
  where
    rest        = filter f xs'
```

`where` lets you pull local variables aside:

* meaning exactly same as `let`, but
* can specify them in _any_ order.

## Anonymous Functions

```ocaml
(* val negate : ('a -> bool) -> 'a -> bool *)
let negate f = fun x -> not (f x)
```

vs

```haskell
negate :: (a -> Bool) -> a -> Bool
negate f = \x -> not (f x)
```

Very similar: `fun` is replaced with `\`


## Tuples and Lists

```ocaml
(* val partition: ('a -> bool) -> 'a list -> ('a list * 'a list) *)
let partition f xs = (filter f xs, filter (negate f) xs)
```

vs

```haskell
partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (negate f) xs)
```

**Note**

`(t1, t2)` instead of `(t1 * t2)`
`[t]`      instead of `t list`


## Larger Example

```ocaml
(* val sort : 'a list -> 'a list *)
let rec sort xs = match xs with
  | []     -> []
  | (h::t) -> let (ls, rs) = partition (fun x -> x < h) t in
              sort ls @ [h] @ sort rs  
```

vs

```haskell
sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (h:t)  = sort ls @ [h] @ sort rs
  where
    (ls,rs) = partition (\x -> x < h) t
```

## List Comprehensions

We can simplify the above, as in Python:

```haskell
sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (h:t)  = sort ls @ [h] @ sort rs
  where
    ls      = [x | x <- t, x <= h]
    ls      = [x | x <- t,  h < x]
```

## Defining Data

```ocaml
type expr
  = Number of float
  | Plus   of expr * expr
  | Minus  of expr * expr
  | Times  of expr * expr
```

vs

```haskell
data Expr
  = Number Double
  | Plus   Expr Expr
  | Minus  Expr Expr
  | Times  Expr Expr
```

## Constructing Data

```ocaml
let ex0 = Number 5.
let ex1 = Plus  (ex0, Number 7.)
let ex2 = Minus (Number 4., Number 2.)
let ex3 = Times (ex1, ex2)
```

vs

```haskell
ex0 = Number 5
ex1 = Plus  ex0 (Number 7)
ex2 = Minus (Number 4) (Number 2)
ex3 = Times ex1 ex2
```

**Note**

The _tags_ `Plus`, `Number` etc. are (constructor) functions

```haskell
Number :: Int -> Expr
Plus,
Minus,
Times  :: Expr -> Expr -> Expr  
```

## Destructing (Accessing) Data

```ocaml
(* val eval: expr -> float *)
let rec eval e = match e with
  | Number n       -> n
  | Plus  (e1, e2) -> eval e1 +. eval e2
  | Minus (e1, e2) -> eval e1 -. eval e2
  | Times (e1, e2) -> eval e1 *. eval e2
```

vs

```haskell
eval :: Expr -> Double
eval (Number    n) = n
eval (Plus  e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2
```

Oh look, we wrote a _compiler_


## Printf Debugging

**Very Very Important**

```ocaml
let fact n = if n <= 0 then 1 else n * fact (n-1)
```

vs.

```haskell
fact n = if n <= 0 then 1 else n * fact (n-1)
```

Q: How to _print out_ each input-output pair for calls to `fact`?

## Printf Debugging

**Very Very Important**

In Ocaml (as in Java, C, Python...), just print it:

```ocaml
let fact n =
  let res = if n <= 0 then 1 else n * fact (n-1)        in
  let _   = Printf.printf "fact n = %d, res = %d\n" n d in
  res
```

But in Haskell, you can't "just" print stuff (for very good reasons...)

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
