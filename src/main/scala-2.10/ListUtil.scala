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

  def reverse(list: List[Any]): List[Any] = {
    if (list.isEmpty)
      List()
    else if (count(list) == 1)
      List(list.head)
    else
      reverse(list.tail) :+ list.head
  }

  /*
      Or simply list == reverse(list) would also work for isPalindrome
  */

  def isPalindrome(list: List[Any]): Boolean = {
    def palindrome(list: List[Any], n: Int): Boolean = {
      if (count(list) == n)
        true
      else
        findNth(list, n) == findNth(list, count(list) - n + 1) &&
          palindrome(list, n + 1)
    }
    palindrome(list, 1)
  }

  def compress(list: List[Any]): List[Any] = {
    def compressor(list: List[Any], leftList: List[Any]): List[Any] = {
      if (list.isEmpty)
        leftList
      else if (leftList.isEmpty)
        list
      else if (findNth(list, count(list)) == leftList.head)
        compressor(list, leftList.tail)
      else
        compressor(list :+ leftList.head, leftList.tail)
    }
    if (list.isEmpty)
      List()
    else
      compressor(List(list.head), list.tail)
  }

  def dropNth(list: List[Any], n: Int): List[Any] = {
    def drop(list: List[Any], leftList: List[Any], i: Int): List[Any] = {
      if (i == n) {
        list ++ leftList.tail
      } else {
        drop(list :+ leftList.head, leftList.tail, i + 1)
      }
    }
    if (count(list) < n || n < 1) {
      throw new RuntimeException(s"Invalid value of n :" + n)
    } else {
      drop(List(), list, 1)
    }
  }
}
