object ListUtil {
  def lastNth(list: List[Int], n: Int): List[Int] = {
    if (list.isEmpty)
       List[Int]()
    else if(list.length == n)
       List[Int](list.head) ++ lastNth(list.tail, n-1)
    else
       lastNth(list.tail, n)
  }
}
