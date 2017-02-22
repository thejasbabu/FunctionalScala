object ListUtil {
  def lastNth(list: List[Int], n: Int): List[Int] = {
    if (list.length < n )
      throw new RuntimeException("Not enough item on the list")
    else if (list.isEmpty)
       List[Int]()
    else if(list.length == n)
       List[Int](list.head) ++ lastNth(list.tail, n-1)
    else
       lastNth(list.tail, n)
  }
}
