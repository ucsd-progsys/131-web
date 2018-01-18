{-@ LIQUID "--no-termination" @-}
module Lec02 where


incr x = x + 1

stincr = \x -> x + 1

eleven = incr (10 + 2)

-- sumList xs = case xs of
               -- []     -> 0
               -- (x:xs) -> x + sumList xs

sumList :: [Int] -> Int
sumList []     = 0
sumList (x:xs) = x + sumList xs

isOdd x = x `mod` 2 == 1

oddsBelow100 = myfilter isOdd [0..100]

myfilter f []      = []
myfilter f (x:xs') = if f x then x : rest else rest
  where
    rest           = myfilter f xs'

neg :: (a -> Bool) -> (a -> Bool)
neg f = \x -> not (f x)

isEven = neg isOdd

partition p xs = (myfilter p xs, myfilter (neg p) xs)



sort []     = []
sort (x:xs) = sort ls ++ [x] ++ sort rs
   where
      ls    = [ y | y <- xs, y < x  ]
      rs    = [ z | z <- xs, x <= z ]


quiz   = [x * 10 | x <- xs, x > 3]
  where
    xs = [0..5]






data Expr
  = Number Double
  | Plus   Expr Expr
  | Minus  Expr Expr
  | Times  Expr Expr






  
