object ListUtil {
  def lastNth(list: List[Int], n: Int): List[Int] = {
    if (count(list) < n)
      throw new RuntimeException("Not enough item on the list")
    else if (list.isEmpty)
      List[Int]()
    else if (count(list) == n)
      List[Int](list.head) ++ lastNth(list.tail, n - 1)
    else
      lastNth(list.tail, n)
  }

  def findNth(list: List[Int], n: Int): Int = {
    if (count(list) < n)
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

  def count(list: List[Int]): Int = {
    if (list.isEmpty)
      0
    else
      count(list.tail) + 1
  }
}
