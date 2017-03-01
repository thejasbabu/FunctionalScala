import org.scalatest.{FlatSpec, Matchers}

class ListUtilTest extends FlatSpec with Matchers {
  "lastNth" should "return last item in the list when n = 1" in {
    val list = List[Int](1, 2, 3, 5, 10)
    val itemList = ListUtil.lastNth(list, 1)
    itemList shouldEqual List[Int](10)
  }

  "lastNth" should "return last two item in the list when n = 2" in {
    val list = List[String]("1", "2", "3", "5", "10")
    val itemList = ListUtil.lastNth(list, 2)
    itemList shouldEqual List[String]("5", "10")
  }

  "lastNth" should "return an empty List if list is empty" in {
    val list = List[Int]()

    val itemList = ListUtil.lastNth(list, 0)
    itemList.length shouldEqual 0
  }

  "lastNth" should "throw an exception when items in the list is lesser than n" in {
    val list = List[Int]()

    val expectedException = intercept[RuntimeException] {
      ListUtil.lastNth(list, 1)
    }

    expectedException.getMessage shouldBe "Not enough item on the list"
  }

  "findNth" should "return the Nth element in the list" in {
    val list = List[Int](1, 2, 3, 5, 10)
    val item = ListUtil.findNth(list, 2)
    item shouldEqual 2
  }

  "findNth" should "throw an exception when invalid value is supplied as N" in {
    val list = List[Int](1, 2)
    val expectedException = intercept[RuntimeException] {
      ListUtil.findNth(list, 0)
    }
    expectedException.getMessage shouldBe "Invalid value for n: 0"
  }

  "findNth" should "throw an exception when list has less elements than the value of N" in {
    val list = List[Int](1, 2)
    val expectedException = intercept[RuntimeException] {
      ListUtil.findNth(list, 3)
    }
    expectedException.getMessage shouldBe "Not enough item on the list"
  }

  "findNth" should "throw an exception when provided with empty list" in {
    val list = List[Int]()
    val expectedException = intercept[RuntimeException] {
      ListUtil.findNth(list, 0)
    }
    expectedException.getMessage shouldBe "No item on the list"
  }

  "count" should "return number of items in list" in {
    val list = List[String]("1", "2")
    val count = ListUtil.count(list)
    count shouldBe 2
  }

  "count" should "return 0 when empty list is passed" in {
    val list = List[Int]()
    val count = ListUtil.count(list)
    count shouldBe 0
  }

  "reverse" should "reverse the elements in the list" in {
    val list = List[Int](1, 2, 3)
    val reversedList = ListUtil.reverse(list)
    reversedList shouldBe List[Int](3, 2, 1)
  }

  "reverse" should "return empty list when input list is empty" in {
    val list = List[String]()
    val reversedList = ListUtil.reverse(list)
    reversedList.length shouldBe 0
  }

  "isPalindrome" should "return true when list is palindrome" in {
    val list = List[Int](1, 2, 3, 3, 2, 1)
    ListUtil.isPalindrome(list) shouldBe true
  }

  "isPalindrome" should "return false when list is not palindrome" in {
    val list = List[Int](1, 2)
    ListUtil.isPalindrome(list) shouldBe false
  }

  "compress" should "remove duplicate and return list" in {
    val list = List[Int](1, 2, 2, 3, 3, 4, 3)
    val compressedList = ListUtil.compress(list)
    compressedList shouldBe List[Int](1, 2, 3, 4, 3)
  }

  "compress" should "return empty list" in {
    val list = List[Int]()
    val compressedList = ListUtil.compress(list)
    compressedList.length shouldBe 0
  }

  "dropNth" should "remove the Nth element from the list" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val modifiedList = ListUtil.dropNth(list, 1)
    modifiedList shouldBe List[Int](2, 3, 4, 5)
  }

  "dropNth" should "throw exception when n is greater than the number of elements in list" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val expectedException = intercept[RuntimeException] {
      ListUtil.dropNth(list, 6)
    }
    expectedException.getMessage shouldEqual "Invalid value of n :6"
  }

  "dropNth" should "throw exception when n is lesser than 1" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val expectedException = intercept[RuntimeException] {
      ListUtil.dropNth(list, 0)
    }
    expectedException.getMessage shouldEqual "Invalid value of n :0"
  }
}
