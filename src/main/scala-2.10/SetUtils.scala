object SetUtils {

  def empty[A](xs: Set[A]): Boolean = xs.size == 0

  def contains[A](xs: Set[A], ele: A): Boolean = {
    def find(set: Set[A]): Boolean = {
      if (empty(set)) false
      else if (set.head == ele) true
      else find(set.tail)
    }
    find(xs)
  }
}
