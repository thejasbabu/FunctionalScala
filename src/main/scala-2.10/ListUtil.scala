object ListUtil {
  def lastOne(list: List[Int]): Int = {
    if (list.isEmpty)
       throw new RuntimeException("Empty list")
    else if(list.length == 1)
        list.head
    else
        lastOne(list.tail)
  }
}
