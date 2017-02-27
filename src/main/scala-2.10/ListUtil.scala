object ListUtil {
  def lastNth(list: List[Int], n: Int): List[Int] = {
    if (list.length < n)
      throw new RuntimeException("Not enough item on the list")
    else if (list.isEmpty)
      List[Int]()
    else if (list.length == n)
      List[Int](list.head) ++ lastNth(list.tail, n - 1)
    else
      lastNth(list.tail, n)
  }

  def findNth(list: List[Int], n: Int): Int = {
    if (list.length < n)
      throw new RuntimeException("Not enough item on the list")
    else if (list.isEmpty)
      throw new RuntimeException("No item on the list")
    else if (n <= 0)
      throw new RuntimeException(s"Invalid value for n: " + n)
    else if (n == 1)
      list.head
    else
      findNth(list.tail, n - 1)
  }
}
