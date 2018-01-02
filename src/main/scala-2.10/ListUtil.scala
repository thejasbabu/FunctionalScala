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

  def findNth(list: List[Any], n: Int): Option[Any] = {
    if (count(list) < n || n <= 0 || list.isEmpty)
      None
    else if (n == 1)
      Some(list.head)
    else
      findNth(list.tail, n - 1)
  }

  val first = (list: List[Any]) => findNth(list, 1)

  val last = (list: List[Any]) => findNth(list, count(list))

  def count(list: List[Any]): Int = {
    def counter(list: List[Any], i: Int): Int = {
      list match {
        case Nil => i
        case _ :: tail => counter(tail, i + 1)
      }
    }
    counter(list, 0)
  }

  def reverse(list: List[Any]): List[Any] = {
    def reverseList(original: List[Any], reversed: List[Any]): List[Any] =
      original match {
        case Nil => reversed
        case x :: Nil => x :: reversed
        case x :: tail => reverseList(tail, x :: reversed)
      }
    reverseList(list, List.empty)
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
      else if (findNth(list, count(list)) == leftList.headOption)
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
