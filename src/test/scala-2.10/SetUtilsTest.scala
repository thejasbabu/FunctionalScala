import org.scalatest.{FlatSpec, Matchers}

class SetUtilsTest extends FlatSpec with Matchers {

  "empty" should "check if set is empty or not" in {
    SetUtils.empty(Set(1, 2, 3)) shouldEqual false
    SetUtils.empty(Set()) shouldEqual true
  }

  "contains" should "check if set contains the element" in {
    SetUtils.contains(Set(1, 2, 3), 2) shouldEqual true
    SetUtils.contains(Set('A', 'B', 'C'), 'D') shouldEqual false
  }

  "union" should "union two sets" in {
    SetUtils.union(Set(1, 2, 3), Set(1, 2, 4)) shouldEqual Set(1, 2, 3, 4)
    SetUtils.union(Set(), Set(1, 2, 4)) shouldEqual Set(1, 2, 4)
    SetUtils.union(Set(1, 2, 4), Set()) shouldEqual Set(1, 2, 4)
  }

  "intersect" should "return intersection of two sets" in {
    SetUtils.intersect(Set(1, 2, 3), Set(1, 2, 4)) shouldEqual Set(1, 2)
    SetUtils.intersect(Set(), Set(1, 2, 4)) shouldEqual Set()
    SetUtils.intersect(Set(1, 2, 4), Set(5, 3)) shouldEqual Set()
  }

  "isSubset" should "check if set is a subset of the superset" in {
    SetUtils.isSubset(Set(1, 2, 3), Set(1, 2)) shouldEqual true
    SetUtils.isSubset(Set(1, 2, 3), Set()) shouldEqual true
    SetUtils.isSubset(Set(1, 2, 3), Set(4)) shouldEqual false
    SetUtils.isSubset(Set(1, 2, 3), Set(4, 2, 1)) shouldEqual false
  }
}
