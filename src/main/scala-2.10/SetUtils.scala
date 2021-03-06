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

  def union[A](s1: Set[A], s2: Set[A]): Set[A] = {
    if (empty(s1)) s2
    else if (contains(s2, s1.head)) union(s1.tail, s2)
    else union(s1.tail, s2 + s1.head)
  }

  def intersect[A](s1: Set[A], s2: Set[A]): Set[A] = {
    def intersectSet(a: Set[A], b: Set[A], res: Set[A]): Set[A] = {
      if (empty(a) || empty(b)) res
      else if (contains(a, b.head)) intersectSet(a, b.tail, res + b.head)
      else intersectSet(a, b.tail, res)
    }
    intersectSet(s1, s2, Set())
  }

  def isSubset[A](superset: Set[A], set: Set[A]): Boolean = {
    empty(set.map(elem => contains(superset, elem)).filter(x => !x))
  }
}
