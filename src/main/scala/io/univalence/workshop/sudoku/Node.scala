package io.univalence.workshop.sudoku

case class Node(value: Int, children: List[Node]) {

  def size: Int =
    children.foldLeft(1) {
      (subsize, child) =>
        subsize + child.size
    }

  def makeString: String =
    children.foldLeft("") {
      (string, child) => string + child.makeString
    } + value

  def makeString2: String =
    Node.makeString2(List(this), "")

}
object Node {
  // @tailrec
  def makeString2(toVisit : List[Node], acc: String): String = {
    if (toVisit.isEmpty) {
      acc
    } else {
      val node = toVisit.head
      val result = node.value.toString
      makeString2(toVisit.tail ++ node.children, acc + result)
    }
  }
}

object NodeMain {
  def main(args: Array[String]): Unit = {
    // val tree = Node(2, Nil)

    val tree =
      Node(1, List(
        Node(2, Nil),
        Node(3, List(
          Node(4, Nil),
          Node(5, Nil),
          Node(8, List(Node(9, Nil))))),
        Node(6, List(
          Node(7, Nil)))))

    println(tree.size)
    println(tree.makeString) // 245983761
    println(tree.makeString2) // 123645879
  }
}