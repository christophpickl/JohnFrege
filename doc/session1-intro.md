Session 1 - Introduction
---

# About

* this is going to be a peer workshop about:
	* learning core functional programming concepts
	* aimed to use that knowledge back in kotlin (improve kotlin code)
	* using frege (a JVM port of haskell which is 99% compatible to haskell)
* we use the vienna university of technology lecture as our basis:
	* http://www.complang.tuwien.ac.at/knoop/fp185A03_ws1718
* the first session will be theory only, with some code we look at
	* only by start of 2nd session we (you) will start coding
	* this session is about getting a rough overview of funcprog/haskell (frege)
	* and about finding out where we are heading to the next time (custom tailored to our needs)
* overall aim:
	* get john ramped up to funcprog, so we are on the same level
	* learn basics and be able to apply it to kotlin (not haskell, but kotlin is the aim!)
	* learn to love funcprog, change the way you think, your mind works
	* get deeper to funcprog (even deeper than kotlin provides) and see its mighty power

## Why is it called John Frege?

![Gottlob_Frege](https://upload.wikimedia.org/wikipedia/commons/thumb/9/99/Young_frege.jpg/220px-Young_frege.jpg)

* It's a combination of you (John) and that guy called [Gottlob Frege](https://en.wikipedia.org/wiki/Gottlob_Frege)
* he was a badass when it comes to philosophy, logic, math and languages
	* one of his most famous work: https://en.wikipedia.org/wiki/Begriffsschrift
* he created the fundamental basis for today's programming languages by formalizing the language
	* he structured sentences in functions and arguments, using precise math/arithmetic expressions
	* an argument can be a function again, leading to functional languages ;)

## Functional programming is fun

* Programming paradigms
	* imperative (procedural, object oriented) VS declarative (logic oriented, functional)
	* the WHAT rather the HOW
		* everything is an expression or declaration, rather a statement
* Why functional?
	* much higher expressiveness of code
	* parallelism (thread safety) comes for free as of no (mutable) state or side-effects
		* as a consequence, calling the very same function more than once, always returns the very same result (making it "idempotent")
		* as there is no state, there are no assignments; question is: how does it play with real-world usecases where there is user input and data storages involved?! (monads to the rescue)
		* expressed mathemtically: `f(x) = f(f(x))` (just like a regular GET/PUT or even a DELETE call should be in ReST)
	* computation as the evaluation of mathematical functions (academic?) => can be formally prooven
		* can be formally verified for correctness, as at its core base there is "pure logic"
* to fuck up your head: funcprog has its origins in the lambda calculus to slve the "entscheidungsproblem" ... but we are not going there (although this is a core concept to understand why funcprog is so powerful and superior than imperative languages)

### Functional concepts

* functions, functionals; curry
* prefix vs infix declaration
* type inference
* recursions! (no more loops)
* lazy evaluation (of expressions)
* data types:
	* lists (comprehension)
	* tupels
	* algebraic data structures (enumerations, product types, sums)
* pattern matching
* (ad hoc) polymorphism
* ... furthermore (not covered):
	* lambda calculus
	* monads (I/O)
	* modularization


# Sample code

## Hello World

Usually haskell got no I/O (side effects), but to fill the gap between our world (containing the concept of time) and functional programming (state- and timeless world), we need some "magic" in frege.

```haskell
main _ = do
    println "Hello, John!"
```

## Comments

```haskell
-- single line
{--
multi
line
--}
```

## Functions

First comes the declaration ("interface") and then the implementation ("class").

```haskell
-- as simple as it can get
staticNumber :: Int
staticNumber = 42

-- take one argument
add2 :: Int -> Int
add2 x = x + 2
-- don't forget paranthesis: println (add2 40)

-- simple addition
add :: Int -> Int -> Int
add x y = x + y
-- same as ... add :: Int -> (Int -> Int)
-- but NOT as ... add :: (Int -> Int) -> Int

-- lets do something useful here
fac :: Int -> Int
fac n = if n == 0 then 1 else n * fac (n - 1)

-- remember the underscore? ;)
nastyAdd :: Int -> (Int -> Int)
nastyAdd x _ = x + 10
```

# Snippets

Language formalized.

```
The capital of Austria is Vienna.

The capital of __ is __.

CapitalOf(x, y): Boolean

f(x) -> y
x e N
y e N

f(3) = 6
... f = x * 2
f('a') = ?

The capital of Austria is Vienna. -- Yes
The capital of Ireland is Vienna. -- No
The capital of Austria is Ireland. -- No?
The capital of Austria is Banana. -- ?

4 + 2 = 6 -- Yes
4 + 2 = 7 -- No
4 + Banana = 7 -- ??
```

Ad-hoc polymorphism.

```
fun add (x, y) = x.add(y)

Int ... add(Int)
String ... add(String)

p1 + p2
2 + 3
f("a" + "b")

f(x) = println x
```

Lazy evaluation.

```
fun f(x: Int, y: Int): Int
f(3, 4+1)
java: f(3, 5)
haskell: f(3, {4+1})

==> logging?!
log.debug("asdf $heavyComputation") ... needed?
log.debug{"asdf $heavyComputation"}
```