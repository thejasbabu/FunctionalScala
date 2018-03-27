sealed trait Tree[A]
case class Node[A](value: A, left: Tree[A], right: Tree[A]) extends Tree[A]
case class EmptyNode[A]() extends Tree[A]

object BST {
  def initialize[A](): Tree[A] = {
    EmptyNode[A]()
  }

  def createNode[A](value: A): Tree[A] = {
    Node[A](value, EmptyNode[A](), EmptyNode[A]())
  }

  def appendNode[A](node: Tree[A], value: A)(implicit numeric: Numeric[A]): Tree[A] = {
    val newNode = createNode(value)
    node match {
      case Node(x, l, EmptyNode()) if numeric.gteq(value, x) => Node(x, l, newNode)
      case Node(x, EmptyNode(), r) if numeric.lteq(value, x) => Node(x, newNode, r)
    }
  }

  def add[A](value: A, node: Tree[A])(implicit numeric: Numeric[A]): Tree[A] = {
    node match {
      case EmptyNode() => createNode(value)
      case Node(x, _, EmptyNode()) if numeric.gteq(value, x) => appendNode(node, value)
      case Node(x, EmptyNode(), _) if numeric.lteq(value, x) => appendNode(node, value)
      case Node(x, l, r) if numeric.gteq(value, x) => Node(x, l, add(value, r))
      case Node(x, l, r) if numeric.lteq(value, x) => Node(x, add(value, l), r)
    }
  }

  def search[A](value: A, node: Tree[A])(implicit numeric: Numeric[A]): Option[Tree[A]] = {
    node match {
      case EmptyNode() => None
      case Node(x, _, _) if numeric.equiv(value, x) => Some(node)
      case Node(x, _, r) if numeric.gt(value, x) => search(value, r)
      case Node(x, l, _) if numeric.lt(value, x) => search(value, l)
    }
  }
}