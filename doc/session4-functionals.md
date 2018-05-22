Session 4 - Functionals
---

*Function references are a cool new feature that can clean up code and do it more semantic.*

*But this comes at the cost of increasing the learning curve. It’s something Java developers are not used to, and because of that, new Kotlin adopters can find them a bit obscure at the beginning.*

[Function references in Kotlin: use functions as lambdas everywhere](https://antonioleiva.com/function-references-kotlin/)

Invoke methods:

```kotlin
// regular
items
    .sortedBy { it.title }
    .map { it.url }
    .forEach { print(it) }

// functional
items
    .sortedBy(MediaItem::title)
    .map(MediaItem::url)
    .forEach(::println)
```

Use function references in combination with nullability:

```kotlin
fun fetchView(): View? = null
fun applyViewChanges(view: View) { }
fun View.let(block: (View) -> Unit) = block(this)

val view = fetchView()
view?.let(::applyViewChanges)
```

# Outline

* Implement some basic functionals yourself in Frege
* Do the very same in Kotlin afterwards
* Implement an HTTP executor with your very own DSL in Kotlin


# Functionals

## Exercises

Implement the following functionals:

1. filter
1. map
1. reverse
1. zip (zipWith?)
1. fold (alias "reduce")
1. giveMeFun (returning functions)

## Modalities

First John is driving and Christoph is navigating in Frege.

Afterwards it is the other way round in Kotlin.

# HTTP Executor

Implement a DSL enabled HTTP request executor in Kotlin. Do it together.

Hint:

```kotlin
request("www.george.at") {
  header {
    acceptJson()
  }
}
```

## Requirements

* mandatory URL
* default GET method
* additional things to set:
	* header
		* creates a new inner DSL, outer scope DSL shouldnt be available (use DSL marker)
		* use infix operator overloading
		* provide handy predefined headers to set
	* body as string


## Optional

* assert on response code (family or custom checker)
* body can be class (internal jackson) or byte array
* base URL
* underlying http client driver (apache, spring, ...)

See: https://github.com/christophpickl/kpotpourri/tree/master/http4k ;)
