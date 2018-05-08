data class Node(
    val value: Int,
    val left: Node? = null,
    val right: Node? = null
)

val graph = Node(6,
    left = Node(4,
        left = Node(3,
            left = Node(8)),
        right = Node(9)),
    right = Node(2,
        left = Node(4),
        right = Node(1,
            right = Node(5))
    ))

//         6
//      4    2
//    3  9  4  1
//  8            5

fun sum(root: Node): Int {
    var sum = 0
    println("currentNode: ${root.value}")
    if (root.left != null) {
        println("going to left")
        sum += sum(root.left)
    }
    if (root.right != null) {
        println("going to right")
        sum += sum(root.right)
    }
    sum += root.value
    return sum
}

println("sum: ${sum(graph)}") // will print 42
