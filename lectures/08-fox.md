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
def tup4(x1, x2, x3, x4):
  (x1, (x2, (x3, (x4, false))))

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

[Click to see video][mov1]

<iframe src="https://www.youtube.com/embed/LddzswC7jEM" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


### Example 2: Garbage in Middle

```python
let y = let tmp = (10, 20)
        in tmp[0] + tmp[1]
  , x = (1, 2)

in
  (x[0] + y, x[1] + y)
```

[Click to see video][mov2]

<iframe src="https://www.youtube.com/embed/2u0y-u1ksbY" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


### Example 3: Garbage in Middle (with stack)

```python
def foo(p, q):
  let tmp = (10, 20)
  in tmp[0] + tmp[1]

let y  = foo(10, 20)
  , x  = (y, y + 1)
  , z  = foo(100, 200)
in
  x[0] + z
```

[Click to see video][mov3]

<iframe src="https://www.youtube.com/embed/pCrBJWDwr2k" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


### Example 4: Transitive Reachability

```python
def range(i, j):
  if (i < j):
    (i, range(i+1, j))
  else:
    false

def sum(l):
  if l == false:
    0
  else:
    l[0] + sum(l[1])

let t1 =
         let l1 = range(0, 3)
         in sum(l1)
  , l  = range(t1, t1 + 3)
in
  (1000, l)
```

[Click to see video][mov4]

<iframe src="https://www.youtube.com/embed/3MEmwxtEprE" width="560" height="315" frameborder="0" allow="autoplay; encrypted-media" allowfullscreen></iframe>


[mov1]: https://youtu.be/LddzswC7jEM
[mov2]: https://youtu.be/2u0y-u1ksbY
[mov3]: https://youtu.be/pCrBJWDwr2k
[mov4]: https://youtu.be/3MEmwxtEprE 


