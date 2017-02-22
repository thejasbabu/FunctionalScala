import org.scalatest.{FlatSpec, Matchers}

class ListUtilTest extends FlatSpec with Matchers {
    "lastOne" should "return last item in the list" in {
      val list = List[Int](1, 2, 3, 5, 10)
      val lastItem = ListUtil.lastOne(list)
      lastItem should equal(10)
    }

    "lastOne" should "throw an exception if an empty list is passed" in {
      val list = List[Int]()
      val expectedException = intercept[RuntimeException] {
        ListUtil.lastOne(list)
      }
      expectedException.getMessage shouldEqual "Empty list"
    }
}
