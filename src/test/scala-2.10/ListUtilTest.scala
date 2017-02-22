import org.scalatest.{FlatSpec, Matchers}

class ListUtilTest extends FlatSpec with Matchers {
    "lastNth" should "return last item in the list when n = 1" in {
      val list = List[Int](1, 2, 3, 5, 10)
      val itemList = ListUtil.lastNth(list, 1)
      itemList shouldEqual List[Int](10)
    }

    "lastNth" should "return last two item in the list when n = 2" in {
      val list = List[Int](1, 2, 3, 5, 10)
      val itemList = ListUtil.lastNth(list, 2)
      itemList shouldEqual List[Int](5, 10)
    }

    "lastNth" should "return an empty List if list is empty" in {
      val list = List[Int]()

      val itemList =  ListUtil.lastNth(list, 1)
      itemList.length shouldEqual 0
    }

    "lastNth" should "throw an exception when items in the list is lesser than n" in {
      val list = List[Int]()

      val expectedException = intercept[RuntimeException] {
        ListUtil.lastNth(list, 1)
      }

      expectedException.getMessage shouldBe "Not enough item on the list"
    }
}
