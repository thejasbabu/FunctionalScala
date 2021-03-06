import org.scalatest.{FlatSpec, Matchers}

class BSTTest extends FlatSpec with Matchers {

  "BST" should "initialize the tree" in {
    BST.initialize[Int]() shouldEqual EmptyNode[Int]()
    BST.initialize[String]() shouldEqual EmptyNode[String]()
  }

  "BST" should "create the leaf nodes" in {
    BST.createNode[Int](1) shouldEqual Node[Int](1, EmptyNode(), EmptyNode())
    BST.createNode[String]("hello") shouldEqual Node[String]("hello", EmptyNode(), EmptyNode())
  }

  "BST" should "append node to tree by creating new leaf node" in {
    BST.appendNode[Int](leaf(4), 5) shouldEqual Node[Int](4, EmptyNode[Int](), leaf(5))
  }

  "BST" should "add node to tree" in {
    val tree = Node[Int](4, leaf(2), leaf(5))
    BST.add[Int](1, tree) shouldEqual Node[Int](4, Node(2, leaf(1), EmptyNode[Int]()), leaf(5))
  }

  "BST" should "search for a node in tree" in {
    val subTree = Node(2, leaf(1), EmptyNode[Int]())
    val tree = Node[Int](4, subTree, leaf(5))
    BST.search[Int](2, tree) shouldEqual Some(subTree)
    BST.search[Int](7, tree) shouldEqual None
  }

  "BST" should "delete a node from the tree" in {
    val subTree = Node(2, leaf(1), EmptyNode[Int]())
    val tree = Node[Int](4, subTree, leaf(5))
    BST.delete[Int](1, tree) shouldEqual Node[Int](4, Node(2, EmptyNode[Int](), EmptyNode[Int]()), leaf(5))
    BST.delete[Int](7, tree) shouldEqual tree
  }

  def leaf[A](value: A):Node[A] = {
    Node(value, EmptyNode[A](), EmptyNode[A]())
  }
}
