Session 2 - Basics
---

# TODO
* workout samples in Kotlin VS Haskell
	* some basic functionals (map, filter, zip, reduce)
	* bigger samples like graph algorithms
* present theory of data structures (node, edge sample), algebraic
* glossary ausarbeiten (first class, higher order, curry, ...)

# Leftovers

...  from previous session

## Lists

```haskell
-- simple declaration:
[ 1, 2, 3]
```

### List comprehension

Create lists by describing what you want (rather the how) and let the computer do the rest.
It is quite similar (the same?) as you would notate it in plain maths.

```haskell
[ x | x <- [1..10]]
[ x | x <- [1..10], x `mod` 2 == 0]
```

See: http://zvon.org/other/haskell/Outputsyntax/listQcomprehension_reference.html

### List functions

* tail
* head
* last
* init

### Functionals

Functionals (a.k.a. "higher order functions") are functions which either take another function as an argument, or return a function as a result. in computer science we also use the term "first-class functions", whereas higher order functions is something coming from the mathematical community.

Please don't mix them up with "top level functions", as they exist in Kotlin. Having functions as a "first class citizen" (function is an object, which was not the case as in Java before java 8; actually, java is pretty much NOT object oriented, there are a lot of trade offs; compare with smalltalk, a pure OO language).

Use paranthesis (like in kotlin) to indicate passing (returning) a lambda. Most famous example:

```haskell
-- map :: (a -> b) -> [a] -> [b]
add2 :: Int -> Int
add2 x = x + 2
map add2 [1..5]

-- filter :: (a -> Bool) -> [a] -> [a]

isEven :: Int -> Bool
isEven x = (x `mod` 2) == 0
filter isEven [1..5]
```

Pay attention that there are no types defined. Ad-hoc polymorphism FTW :)

### Pattern Matching

This is like Java's `switch` but far far far better. It's like Kotlin's `when` but even far better that this.

```haskell
-- replace if/else cascades with patterns
weekday :: Int -> String
weekday 1 = "Mon"
weekday 2 = "Tue"
weekday _ = "?"

-- use recursion and pattern matching to sum a list
mySum :: [Int] -> Int
mySum x = mySum' x 0

mySum' :: [Int] -> Int -> Int
mySum' (x:xs) res = mySum' xs (res + x)
mySum' [] res = res

-- or we can go even further and only allow types which are valid operands for the + infix-operator
```

Be aware that in combination with data types (and polymorphism and its powerful type system), this gets pretty awesome!

## Advanced stuff

### Quicksort

See slides: p.79

Functional approach with Haskell:

```haskell
quickSort :: [Integer] -> [Integer]
quickSort [] = []
quickSort (n:ns) = quickSort [ m | m <- ns, m <= n ] ++ [n] ++ quickSort[m | m<-ns, m>n]
```

Imperative approach with Java:

```java
public class Quicksort  {
    private int[] numbers;
    private int number;

    public void sort(int[] values) {
        if (values ==null || values.length==0){
            return;
        }
        this.numbers = values;
        number = values.length;
        quicksort(0, number - 1);
    }

    private void quicksort(int low, int high) {
        int i = low, j = high;
        int pivot = numbers[low + (high-low)/2];

        while (i <= j) {
            while (numbers[i] < pivot) {
                i++;
            }
            while (numbers[j] > pivot) {
                j--;
            }
            if (i <= j) {
                exchange(i, j);
                i++;
                j--;
            }
        }
        if (low < j)
            quicksort(low, j);
        if (i < high)
            quicksort(i, high);
    }

    private void exchange(int i, int j) {
        int temp = numbers[i];
        numbers[i] = numbers[j];
        numbers[j] = temp;
    }
}
```

### Lazy evaluation

we usually are used to eager evaluation, whereas haskell is lazy evaluated. as there is no state (no side effects) that's perfectly fine, and saves from computing values which are not needed anyway (performance boost!). we don't have to do anything whatsoever to gain this power, it is right builtin into haskell, completely transparent to us:

```haskell
fun42 :: Int
fun42 = 42

infiniteFun :: Int -> Int
infiniteFun x = infiniteFun 1

lazyFun :: Bool -> Int -> Int -> Int
lazyFun True x _ = x
lazyFun False _ y = y

mainFun :: Int
mainFun = lazyFun True fun42 (infiniteFun 0)
-- make it run infinitely ;)
--mainFun = lazyFun False fun42 (infiniteFun 0)

main _ = do
	println "maybe infinite..."
	println mainFun
	println "good luck, finished :)"
```

as you can see, the third argument of `lazyFun` will only be determined (evaluted) if needed. when the `infiniteFun` is invoked, it will just call itself recursively, and as we all know only chuck norris can make them return a value :) but magically, with haskell we are as powerful as him, so we can make trick the system and simply dont touch that nasty expression.


# Setup

## Tools

* Good eclipse plugin, weak intellij plugin
* Maven and Gradle support
	* Last commit for Gradle plugin 2016 :( https://github.com/Frege/frege-gradle-plugin


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