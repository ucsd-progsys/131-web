---
title: Garbage Collection
date: 2018-03-7
headerImg: fox.jpg
---

![Compiler and Runtime](/static/img/compiler-pipeline-1-2.png)

## Recap: Tuples on the Heap

### QUIZ

Using the above "library" we can write code like:

```haskell
def get(e, i):
  if (i == 0):
    head(e)
  else:
    get(tail(e), i-1)

let quad = tup4(1, 2, 3, 4) in
  get(quad, 0) + get(quad, 1) + get(quad, 2) + get(quad, 3)
```

What will be the result of compiling the above?

1. Compile error
2. Segmentation fault
3. Other run-time error
4. `4`
5. `10`




### QUIZ

Using the above "library" we can write code like:

```haskell
let quad = tup4(1, 2, 3) in
  get(quad, 0) + get(quad, 1) + get(quad, 2) + get(quad, 3)
```

What will be the result of compiling the above?

1. Compile error
2. Segmentation fault
3. Other run-time error
4. `4`
5. `10`


### Example 1: Garbage at End

```python
let x = (1, 2)
  , y = let tmp = (10, 20)
        in tmp[0] + tmp[1]
in
  (x[0] + y, x[1] + y)
```

### Example 2: Garbage in Middle

```python
let y = let tmp = (10, 20)
        in tmp[0] + tmp[1]
  , x = (1, 2)

in
  (x[0] + y, x[1] + y)
```

### Example 3: Garbage at End (with stack)

```python
def foo(p, q):
  let tmp = (p, q)
  in tmp[0] + tmp[1]

let x = (1, 2)
  , y = foo(10, 20)
in
   (x[0] + y, x[1] + y)
```

### Example 4: Garbage in Middle (with stack)

```python
def foo(p, q):
  let tmp = (p, q)
  in tmp[0] + tmp[1]

let y = foo(10, 20)
  , x = (1, 2)
in
   (x[0] + y, x[1] + y)
```

### Example 5: Transitive Reachability

```python
def range(i, j):
  if (i < j):
    (i, range(i+1, j))
  else:
    false

def sum(acc, l):
  if l == false:
    acc  
  else:
    sum(acc + l0, l[1])

let l    = range(1, 3)
  , tmp1 = let l1 = (10, l0)
           in sum(0, l1)
  , l0   = (0, l)
  , tmp2 = let l2 = (100, l0)
           in sum(0, l2)
in
  (tmp1 + tmp2, l)
```


### Example 6: Transitive Reachability (with Stack)

```python
def range(i, j):
  if (i < j):
    (i, range(i+1, j))
  else:
    false

def sum(acc, l):
  if l == false:
    acc  
  else:
    sum(acc + l0, l[1])

def foo(x, l):
  let tmp = (x, l)
  in sum(0, tmp)

let l    = range(1, 3)
  , tmp1 = foo(10,  l)
  , l0   = (0, l)
  , tmp2 = foo(100, l)
in
  (tmp1 + tmp2, l)
```

## TODO

- Heaps for above examples  
- QUIZ: which allocation will trigger GC?

- Mark
- QUIZ: which is marked?

- Forward
- QUIZ: what is forwarding address?

- Redirect
- QUIZ: why not redirect DURING forward?

- Compact
- QUIZ: ??
