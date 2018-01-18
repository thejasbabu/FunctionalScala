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
}
