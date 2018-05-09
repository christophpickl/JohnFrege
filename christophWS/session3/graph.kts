data class Node<T>(
    val value: T,
    val left: Node<T>? = null,
    val right: Node<T>? = null
)
val graphAsString = """
          6
        /   \
       4     2
      / \   /  \
     3   9 4    1
    /            \
   8              5
"""

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

fun sum(root: Node<Int>): Int {
    var sum = 0
    if (root.left != null) {
        sum += sum(root.left)
    }
    if (root.right != null) {
        sum += sum(root.right)
    }
    sum += root.value
    return sum
}

println(sum(graph))
