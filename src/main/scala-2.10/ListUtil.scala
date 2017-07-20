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
    def counter(list: List[Any], i: Int): Int = {
      if (list.isEmpty)
        i
      else
        counter(list.tail, i + 1)
    }
    if (list.isEmpty)
      0
    else
      counter(list.tail, 1)
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
    if (count(list) < n || n < 1) {
      throw new RuntimeException(s"Invalid value of n")
    } else {
      val (firstList, secondList) = split(list, n - 1)
      firstList ++ secondList.tail
    }
  }

  def split(list: List[Any], n: Int): (List[Any], List[Any]) = {
    def splitAtNth(list: List[Any], leftList: List[Any]): (List[Any], List[Any]) = {
      if (count(list) == n || n < 1) {
        (list, leftList)
      } else {
        splitAtNth(list :+ leftList.head, leftList.tail)
      }
    }
    if (count(list) < n) {
      throw new RuntimeException(s"Invalid value of n")
    } else {
      splitAtNth(List(), list)
    }
  }

  def slice(list: List[Any], i: Int, j: Int): List[Any] = {
    if (count(list) < j || i < 1) {
      throw new RuntimeException(s"Invalid value of n")
    } else {
      val (firstList, _) = split(list, j)
      val (_, resultList) = split(firstList, i - 1)
      resultList
    }
  }
}
