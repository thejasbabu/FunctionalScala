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
}
