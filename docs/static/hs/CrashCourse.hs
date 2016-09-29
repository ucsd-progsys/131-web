module CrashCourse where

{-
[a] -> [b] -> [(a, b)]

[Int] -> [String] -> [(Int, String)]
mystery [1,2,3] ["cat", "dog", "jorse"]
  = [(1, "cat"), (2, "dog"), (3, "jorse")]

[Int] -> [Int] -> [(Int, Int)]
mystery [1,2,3]  [5,6,7]
  = [(1, 5), (2, 6), (3, 7)]

-}
import Text.Printf (printf)
import Debug.Trace (trace)
import Prelude hiding (negate, filter)

{-
(* val incr : int -> int *)
let incr x = x + 1

let eleven = incr 10
-}

incr :: Int -> Int
incr x = x + 1

-- eleven = incr 10


{-
(* val listSum : int list -> int list *)
let rec listSum xs = match xs with
  | []       -> 0
  | (x::xs') -> x + listSum xs'
-}


listSum :: [Int] -> Int
listSum xs = case xs of
               []    -> 0
               x:xs' -> x + listSum xs'


listSum' :: [Int] -> Int
listSum' []     = 0
listSum' (x:xs) = x + listSum' xs


{-
(* val filter : ('a -> bool) -> 'a list -> 'a list *)
let filter f xs = match xs with
  | []     -> []
  | x::xs' -> let rest = filter f xs' in
              if f x then x :: rest else rest
-}

{-
filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) let rest = filter f xs' in
                if f x then x:rest else rest

filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) let rest = filter f xs' in
                if f x then x:rest else rest
-}

filter :: (a -> Bool) -> [a] -> [a]
filter f []     = []
filter f (x:xs) = if f x then x:rest else rest
  where
    rest        = filter f xs


{-
(* val negate : ('a -> bool) -> 'a -> bool *)
let negate f = fun x -> not (f x)
-}

negate :: (a -> Bool) -> a -> Bool
negate f = \x -> not (f x)

{-
(* val partition: ('a -> bool) -> 'a list -> ('a list * 'a list) *)
let partition f xs = (filter f xs, filter (negate f) xs)
-}

partition :: (a -> Bool) -> [a] -> ([a], [a])
partition f xs = (filter f xs, filter (negate f) xs)

{-
(* val sort : 'a list -> 'a list *)
let rec sort xs = match xs with
  | []     -> []
  | (h::t) -> let (ls, rs) = partition (fun x -> x < h) t in
              sort ls @ [h] @ sort rs
-}

sort :: (Ord a) => [a] -> [a]
sort []     = []
sort (h:t)  = sort ls ++ [h] ++ sort rs
  where
    (ls,rs) = partition (\x -> x < h) t


-- | Comprehensions

sort' :: (Ord a) => [a] -> [a]
sort' []    = []
sort' (h:t) = sort' ls ++ [h] ++ sort' rs
  where
    ls      = [x | x <- t, x <= h]
    rs      = [x | x <- t,  h < x]


-- | Constructors
{-
type expr
  = Number of float
  | Plus   of expr * expr
  | Minus  of expr * expr
  | Times  of expr * expr
-}

data Expr
  = Number Double
  | Plus   Expr Expr
  | Minus  Expr Expr
  | Times  Expr Expr
  deriving (Show)
{-
let ex0 = Number 5.1
let ex1 = Plus  (ex0, Number 7.2)
let ex2 = Minus (Number 4.3, Number 2.9)
let ex3 = Times (ex1, ex2)
-}

ex0 = Number 5
ex1 = Plus  ex0 (Number 7)
ex2 = Minus (Number 4) (Number 2)
ex3 = Times ex1 ex2

-- | Destructors

{-
(* val eval: expr -> float *)
let rec eval e = match e with
  | Number n       -> n
  | Plus  (e1, e2) -> eval e1 +. eval e2
  | Minus (e1, e2) -> eval e1 -. eval e2
  | Times (e1, e2) -> eval e1 *. eval e2
-}


eval :: Expr -> Double
eval (Number    n) = n
eval (Plus  e1 e2) = eval e1 + eval e2
eval (Minus e1 e2) = eval e1 - eval e2
eval (Times e1 e2) = eval e1 * eval e2

-- | Recursive Functions

{-
let rec fact n
  = let res = if n <= 0 then
                1
              else
                fact (n-1)
    in
       Printf.printf ""

let rec fact n =
  let res = if n <= 0 then 1 else fact (n-1)          in
  let _   = Printf.printf "fact n = %d, res = %d\n" n res in
  res

-}

fact :: Int -> Int
-- fact n = if n <= 0 then 1 else n * fact (n-1)
fact n
  | n <= 0    = 1
  | otherwise = fact (n-1)

-- trace :: String -> a -> a
fact' :: Int -> Int
fact' n  = trace msg res
  where
    msg = printf "fact n = %d, res = %d\n" n res
    res = if n <= 0 then 1 else n * fact' (n-1)

-- | Printf Debugging

{-
let fact n =
  let res = if n <= 0 then 1 else n * fact (n-1)        in
  let _   = Printf.printf "fact n = %d, res = %d\n" n d in
  res
-}

--------------------------------------------------------------------------------

{- CASE STUDY 1 -}
digitsOfInt :: Int -> [Int]
digitsOfInt n
  | n < 0      = []
  | n < 10     = [n]
  | otherwise  = digitsOfInt (n `div` 10) ++ [mod n 10]


-- pipe [f1,...,fn] x  = f1(f2(...(fn x)))

pipe :: [a -> a] -> a -> a
pipe = foldr (.) id

{- CASE STUDY 2


foo x     = e
foo       = \x -> e

let foo x = e
let foo   = fun x -> e
-}
