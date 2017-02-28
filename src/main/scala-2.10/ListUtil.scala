object ListUtil {
  def lastNth(list: List[Any], n: Int): List[Any] = {
    if (count(list) < n)
      throw new RuntimeException("Not enough item on the list")
    else if (list.isEmpty)
      List[Any]()
    else if (count(list) == n)
      List[Any](list.head) ++ lastNth(list.tail, n - 1)
    else
      lastNth(list.tail, n)
  }

  def findNth(list: List[Any], n: Int): Any = {
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

  def count(list: List[Any]): Int = {
    if (list.isEmpty)
      0
    else
      count(list.tail) + 1
  }
}
