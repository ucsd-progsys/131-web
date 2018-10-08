---
title: Hello, world! 
headerImg: Eiffel.png
date: 2018-09-28
---


## What *is* a Compiler?

A function that maps an _input_ string to an _output_ string.

```haskell
compiler :: String -> String
```

Typically, the _input_ and _output_ strings are _"programs"_

```haskell
compiler :: SourceProgram -> TargetProgram
```

For example, here are some well-known _compilers_

```haskell
gcc, clang :: C          -> Binary          -- a.out, .exe
ghc        :: Haskell    -> Binary                 
javac      :: Java       -> JvmByteCode     -- .class
scalac     :: Scala      -> JvmByteCode      
ocamlc     :: Ocaml      -> OcamlByteCode   -- .cmo
ocamlopt   :: Ocaml      -> Binary               
gwt        :: Java       -> JavaScript      -- .js
v8         :: JavaScript -> Binary
nasm       :: X86        -> Binary    
pdftex     :: LaTeX      -> PDF
pandoc     :: Markdown   -> PDF | Html | Doc  
```

Key Requirements on output program:

1. Has the _same meaning_ ("semantics") as input,
2. Is _executable_ in relevant _context_ (VM, microprocessor, web browser).

### A Bit of History

Compilers were invented to [avoid writing machine code by hand][soap-fortran-assembly]

![From Binary to FORTRAN](/static/img/binary-soap-fortran.png)

Richard Hamming -- The Art of Doing Science and Engineering, p25:

> In the beginning we programmed in absolute binary...
> Finally, a Symbolic Assembly Program was devised --
> after more years than you are apt to believe during
> which most programmers continued their heroic absolute
> binary programming. At the time [the assembler] first
> appeared I would guess about 1% of the older programmers
> were interested in it -- using [assembly] was "sissy stuff",
> and a real programmer would not stoop to wasting machine
> capacity to do the assembly.

John A.N. Lee, Dept of Computer Science, Virginia Polytechnical Institute

> One of von Neumann's students at Princeton recalled that
> graduate students were being used to hand assemble programs
> into binary for their early machine. This student took time
> out to build an assembler, but when von Neumann found out
> about it he was very angry, saying that it was a waste of
> a valuable scientific computing instrument to use it to do
> clerical work.


### What does a Compiler *look like*?

![Compiler Pipeline](/static/img/compiler-pipeline.png)

An input source program is converted to an executable binary in many stages:

* **Parsed** into a data structure called an **Abstract Syntax Tree**
* **Checked** to make sure code is well-formed (and well-typed)
* **Simplified** into some convenient **Intermediate Representation**
* **Optimized** into (equivalent) but faster program
* **Generated** into assembly `x86`
* **Linked** against a run-time (usually written in C)

### What is CSE 131 ?

* A *bridge* between two worlds

  * _High-level:_ ML        (**CSE 130**)
  * _Machine Code:_ X86/ARM (**CSE  30**)

A sequel to both those classes.

* How to write **a compiler** for `NanoML -> X86`

  1. Parsing
  2. Checking & Validation
  3. Simplification & Normalizing
  4. Optimization
  5. Code Generation


* But also, how to write **complex programs**

  1. Design
  2. Implement
  3. Test
  4. **Iterate**

### How write a Compiler?

General recipe, applies to any large system

+ *gradually, one feature at a time!*

We will

* **Step 1** Start with a teeny tiny language,
* **Step 2** Build a full compiler for it,
* **Step 3** Add a few features,
* **Go  to** Step 2.

(Yes, loops forever, but we will hit Ctrl-C in 10 weeks...)

## Mechanics

### Who *are* we?

**Prof:** 

* Ranjit Jhala

**TAs:** 

* Marc Andrysco
* Gary Soeller
* Anish Tondwalkar

### How will we *grade*?

**(30%) Assignments**

+ 7 assignments, [first one is up, due 10/5](https://classroom.github.com/a/OjQrcTj6)
+ All programming
+ Groups of upto 2


**(30%) Midterm (11/2)**

+ In-class  
+ 2-sided printed "cheat sheet"


**(35%) Final (12/14)**

+ 2-sided printed "cheat sheet"

**(05%) Clickers**

+ *Attempting to answer* > 75% questions
+ Starting from _next week_ 

**(05%) Piazza Extra Credit**

+ To top-20 best participants


## Course Outline

## What will *we do* ?

Write **a compiler** for `NanoML -> X86`

But Rome wasn't built in a day ... and neither is any serious software.

![Rome wasn't built in a day](/static/img/Eiffel.jpg)

So we will write _many_ compilers:

* Numbers and increment/decrement
* Local Variables
* Nested Binary Operations
* Booleans, Branches and Dynamic Types
* Functions
* Tuples and Structures
* Lambdas and closures
* Types and Inference
* Garbage Collection

## What will *you learn* ?

**Core principles of compiler construction**

* Managing Stacks & Heap
* Type Checking
* Intermediate forms
* Optimization

**Several new languages**

* `Haskell` to write the compiler
* `C`       to write the "run-time"
* `X86`     compilation target

**_More importantly_ how to write a large program**

* How to use types for design
* How to add new features / refactor
* How to test & validate   


## What do you *need to know* ?

This 131 **depends very heavily** on CSE 130

* Familiarity with Functional Programming and Ocaml
* Datatypes (e.g. Lists, Trees, ADTs)
* Polymorphism
* Recursion
* HOFs (e.g. `map`, `filter`, `fold`)

Also **depends on** CSE 30

* Experience with some `C` programming
* Experience with some assembly (`x86`)

### A few words on the medium of instruction

We will use [Haskell](https://haskell-lang.org/) which, for our
purposes is like Ocaml but with nicer syntax.

Haskell has many advanced features beyond what we saw in 130,
but we won't be using them; in the few cases we do, I'll explain
them as we go.

Here are some links to get you started:

* [Haskell for Ocaml Programmers][haskell-for-ocamlers]
* [Hoogle: Type-based API Search][hoogle]

### Lets Go! 


[hoogle]: https://www.haskell.org/hoogle/
[haskell-for-ocamlers]: http://science.raphael.poss.name/haskell-for-ocaml-programmers.html
[soap-fortran-assembly]: http://worrydream.com/dbx/



["cat", "dog", "horse"]   -- :: [String]  -- words 
                                String    -- separator
"cat;dog;horse"           -- :: String    -- "crunche words with separator"
