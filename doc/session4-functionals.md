Session 4 - Functionals
---

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
