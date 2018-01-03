object ListUtil {

  def first[A](list: List[A]): Option[A] = findNth(list, 1)

  def last[A](list: List[A]): Option[A] = findNth(list, count(list))

  def isEmpty[A](list: List[A]): Boolean = if (count(list) == 0) true else false

  def lastNth[A](list: List[A], n: Int): List[A] = {
    if (count(list) < n)
      throw new RuntimeException("Not enough item on the list")
    else if (isEmpty(list))
      List[A]()
    else if (count(list) == n)
      List[A](list.head) ++ lastNth(list.tail, n - 1)
    else
      lastNth(list.tail, n)
  }

  def findNth[A](list: List[A], n: Int): Option[A] = {
    if (count(list) < n || n <= 0 || isEmpty(list))
      None
    else if (n == 1)
      Some(list.head)
    else
      findNth(list.tail, n - 1)
  }

  def count[A](list: List[A]): Int = {
    def counter(list: List[A], i: Int): Int = {
      list match {
        case Nil => i
        case _ :: tail => counter(tail, i + 1)
      }
    }
    counter(list, 0)
  }

  def reverse[A](list: List[A]): List[A] = {
    def reverseList(original: List[A], reversed: List[A]): List[A] =
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

  def isPalindrome[A](list: List[A]): Boolean = {
    def palindrome(list: List[A], n: Int): Boolean = {
      if (count(list) == n)
        true
      else
        findNth(list, n) == findNth(list, count(list) - n + 1) &&
          palindrome(list, n + 1)
    }
    palindrome(list, 1)
  }

  def compress[A](list: List[A]): List[A] = {
    def compressor(compressed: List[A], original: List[A]): List[A] = {
      if (isEmpty(original))
        compressed
      else if (last(compressed) == first(original))
        compressor(compressed, original.tail)
      else
        compressor(compressed :+ original.head, original.tail)
    }
    compressor(List(), list)
  }

  def dropNth[A](list: List[A], n: Int): List[A] = {
    if (count(list) < n || n < 1) {
      throw new RuntimeException(s"Invalid value of n")
    } else {
      val (firstList, secondList) = split(list, n - 1)
      firstList ++ secondList.tail
    }
  }

  def split[A](list: List[A], n: Int): (List[A], List[A]) = {
    def splitAtNth(list: List[A], leftList: List[A]): (List[A], List[A]) = {
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

  def slice[A](list: List[A], i: Int, j: Int): List[A] = {
    if (count(list) < j || i < 1) {
      throw new RuntimeException(s"Invalid value of n")
    } else {
      val (firstList, _) = split(list, j)
      val (_, resultList) = split(firstList, i - 1)
      resultList
    }
  }
}
