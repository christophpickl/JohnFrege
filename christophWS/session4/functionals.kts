
// ============================================================================

println("Filter: " + myFilter(::isEven, 1.rangeTo(5).toList()))

fun isEven(x: Int): Boolean = x % 2 == 0

fun <T> myFilter(f: (T) -> Boolean, list: List<T>): List<T> {
    val result = mutableListOf<T>()
    list.forEach {
        if (f(it)) {
            result += it
        }
    }
    return result
}

// ============================================================================

println("Mapped: " + myMap(::timesTwo, 1.rangeTo(5).toList()))

fun timesTwo(x: Int) = x * 2

fun <A, B> myMap(f: (A) -> B, list: List<A>): List<B> {
    val result = mutableListOf<B>()
    list.forEach {
        result += f(it)
    }
    return result
}

// ============================================================================

println("Reversed: " + myReverse(1.rangeTo(5).toList()))

fun <T> myReverse(list: List<T>): List<T> {
    val result = mutableListOf<T>()
    list.size.downTo(1).forEach {
        result += list[it - 1]
    }
    return result
}

// ============================================================================

println("Zip: " + myZip(1.rangeTo(5).toList(), listOf("a", "b", "c")))

fun <A, B> myZip(a: List<A>, b: List<B>): List<Pair<A, B>> {
    val result = mutableListOf<Pair<A, B>>()
    1.rangeTo(Math.min(a.size, b.size)).forEach {
        result += Pair(a[it - 1], b[it - 1])
    }
    return result
}

// ============================================================================

println("Fold L: " + myFoldl(::minus, 0, 1.rangeTo(5).toList()))
println("Fold R: " + myFoldr(::minus, 0, 1.rangeTo(5).toList()))

fun minus(x: Int, y: Int) = x - y

fun <A, B> myFoldl(f: (A, B) -> A, initial: A, list: List<B>): A {
    var result = initial
    list.forEach {
        result = f(result, it)
    }
    return result
}

fun <A, B> myFoldr(f: (A, B) -> B, initial: B, list: List<A>): B {
    var result = initial
    list.forEach {
        result = f(it, result)
    }
    return result
}

// ============================================================================

println("Some fun 2: " + giveMeFun(2)(1))
println("Some fun 3: " + giveMeFun(3)(1))

fun giveMeFun(x: Int): (Int) -> Int =
    if (isEven(x)) { y -> x + y}
    else { y -> x - y }
