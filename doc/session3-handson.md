Session 3 - Hands On
---

# Outline

* setup IDE
* quicksort
* theory for upcoming exercise
* pair the exercise :)

# Setup FregIDE

## Bug

The last time we've got a nasty error when trying to install the FregIDE plugin saying:

```
An error occurred while collecting items to be installed
session context was:(profile=C__Users_foobar_eclipse_java-oxygen_eclipse, phase=org.eclipse.equinox.internal.p2.engine.phases.Collect, operand=, action=).
Artifact not found: http://www.frege-lang.org/fregide/plugins/impulse_0.1.0.201804202307.jar.
http://www.frege-lang.org/fregide/plugins/impulse_0.1.0.201804202307.jar
```

They seem to have fixed the issue, as the JAR is available again: http://www.frege-lang.org/fregide/plugins/impulse_0.1.0.201804202307.jar

## Installation

* download latest eclipse and configure it properly
	* increase memory settings in `eclipse.ini`:
	```
	-vmargs
	-Dosgi.requiredJavaVersion=1.8
	-Xss4m
	-Xms400m
	-Xmx2048m
	```
	* use UTF-8 encoding
	* use spaces instead of tabs
	* add "CMD+R" run shortcut ;)
* install the eclipse plugin
	* basically just an update site pointing to:  http://www.frege-lang.org/fregide/
	* or follow the complete tutorial: https://github.com/Frege/eclipse-plugin
	* PS: eclipse plugin is far superior than working with intellij
* create frege project:
	* hit New / Other
	* in the dialog select Frege Project
	* enter some arbitrary name and hit finish
* create and run file:
	* create a file `hello.fr` in the `src/` folder
	```
	module HelloFrege where

	main _ = do
    	println "Hello Frege"
    ```
    * create a new run configuration:
    	* select Java Application
    	* the Main class has to be the same name as the module name defined in the frege file, which is in our case: `HelloFrege`
    * hit run and you should see the text in the console, bravo!

# Code Highlights

## Typeclasses

Define your own types:

```haskell
data Bool = False | True  
```

The `False` and `True` part are so-called value constructors. It can be thought of an enum, with a fixed set of values each not instantiable and not carrying any state.

```haskell
data Shape = Circle Float | Rectangle Float Float
```

Actually value constructors are just regular functions taking arguments (parameters) and construct the (data) type.
Operating on a `Shape` now becomes funny:

```haskell
surface :: Shape -> Float  
surface (Circle r) = pi * r ^ 2  
surface (Rectangle w h) = w * h
```

Using good old pattern matching we do some kind of "instance of" check. Based on the runtime type, haskell will determine which implementation to use. And same as in kotlin, the moment we checked the runtime type the compiler knows the available properties (smart casting), unlike in java where you have to cast it to the type although the compiler already should know.


PS: if we want to print those things out, haskell needs a way to know how its string representation looks like. This can be easily done by letting it "implement interfaces" like so: `data Shape = Circle Float | Rectangle Float Float deriving (Show)`


Finally i'd like to show you how to introduce boundaries for a type parameter, so consider this:

```haskell
data Container a = Empty | Full a

addIt :: Num a => Container a -> Container a -> Container a
addIt (Full x) (Full y) = Full (x + y)
addIt x Empty = x
addIt Empty y = y

instance (Show a) => Show (Container a) where
    show (Empty) = "Empty"
    show (Full a) = "Full " ++ (show a)

main :: IO ()
main = do
  -- putStrLn $ show $ (Full "a")
  putStrLn $ show $ (addIt (Full 22) (Full 20))
```

### Extra info

Some academic theory: Similar to kotlin's sealed classes, so-called **algebraic datastructures** are known from math and simply put, they state that there is a set of things,
whereas the things contain a defined set of elements and a defined set of operations which can act on them.

A sample from math would be: `<N; +, -, *, />` (all natural numbers and the four basic arithmic operators)

Oh, and don't forget about "GADs", **G**eneralised **A**lgebraic **D**atatypes, which are... kind a funny too ;) But usually you don't want to restrict your data types that hard and rather do it via polymorphism.

## Polymorphism

__Parametric polymorphic__ functions in haskell look like this:

```haskell
length :: [a] -> Int
fst :: (a, b) -> a
snd :: (a, b) -> b
map :: (a -> b) -> [a] -> [b]
```

Usually type variables are lowercased like `a`, whereas specific types are upper cased like `Int`.

__Ad-hoc polymorphism__ has a *bound type*, and there are different implementations with different types (a.k.a. "function overloading").
A well known example is the plus operator: `2 + 3 = 5` and `'2' + '3' = '23'`. In haskell we use __type classes__ to provide this kind of polymorphism, something we have learned just recently :)

ATTENTION: Java can only do this for arguments, whereas haskell can do it for arguments AND return types (and combination of both).

In the object oriented world we usually do something like __subtyping polymorphism__ where the subclass changes the implementation/behaviour of the superclass. This is done at runtime, unlike ad-hoc polymorphism which can be determined by the compiler (for parametric polymorphism there is only one way to go anyway).

More to read: https://izbicki.me/blog/polymorphism-in-haskell-vs-c%2B%2B.html

## Extra: Multiple implementations 

Naive implementation:

```haskell
weekdays :: Int -> [Char]
weekdays 1 = "Mon"
...
weekdays 7 = "Sun"
weekdays _ = "?"
```

Optimatized implementation:

```haskell
weekdays :: Int -> [Char]
weekdays x
    | x == 1 = "Mon"
    | ...
    | x == 7 = "Sun"

```

But what about passing a number `x < 1 || x > 7`?

Ooops... it will fail with an runtime (!) error, saying: `samples: samples.hs:(3,1)-(7,22): Non-exhaustive patterns in function weekdays`

Any solutions to that problem? Is this pattern matching approach with pipes the wrong thing, or can it be used somehow else? Should we use enumerations instead?

# Exercises

## I/O

The main method looks like this:

```haskell
main :: IO ()
main = do
  putStrLn "foobar"
```

Printing is something which has side-effects and is not truely functional anymore,
but as we deal with the real world, and don't want to get stuck in an academic utopia,
we need to face the fact that the world indeed got the concept of time and state.

## Graph Theory

Given the following graph:

```
          6
        /   \
       4     2
      / \   /  \
     3   9 4    1
    /            \
   8              5
```

Compute the sum of all nodes.

Do it first in

1. Kotlin, and then in
2. Haskell (Frege)

# Appendix

## Notes

Pattern matching with data classes can also be done in Kotlin with sealed classes:

```kotlin
sealed class Shape {
  class Circle(val r: Float) : Shape()
  class Rectangle(val w: Float, val h: Float) : Shape()
}

fun surface(shape: Shape): Float = when(shap) {
  is Circle -> pi * shape.r
  is Rectangle -> shape.w * shape.h
}
```

In order to print stuff without all those annoying parenthesis, you can use the dollar symbol instead:

```haskell
putStrLn $ show $ (addIt (Full 22) (Full 20))
putStrLn (show (addIt (Full 22) (Full 20)))
```


## Fat Sample

Go through this on your own if you are brave enough ;)

```haskell
data ClassRoom = ClassRoom {className::String, studentCount::Int}
data SportsTeam =  SportsTeam {teamName::String, memberCount::Int}

class Validatable a where
  isValid :: a -> Bool

instance Validatable ClassRoom where
  isValid v =  studentCount v > 0

instance Validatable SportsTeam where
  isValid v =  memberCount v > 0 && memberCount v < 10

main :: IO ()
main = do
  let class_room = ClassRoom {className="FuncProg", studentCount=2}
      sports_team = SportsTeam {teamName="Fat Bastards", memberCount=6}
  putStrLn $ show $ isValid class_room
  putStrLn $ show $ isValid sports_team
```

Source: http://devanla.com/polymorphism-in-haskell-1.html