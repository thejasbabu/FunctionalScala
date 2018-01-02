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
    val itemList = ListUtil.lastNth(List[Int](), 0)
    itemList.length shouldEqual 0
  }

  "lastNth" should "throw an exception when items in the list is lesser than n" in {
    val expectedException = intercept[RuntimeException] {
      ListUtil.lastNth(List[Int](), 1)
    }

    expectedException.getMessage shouldBe "Not enough item on the list"
  }

  "findNth" should "return the Nth element in the list" in {
    val list = List[Int](1, 2, 3, 5, 10)
    ListUtil.findNth(list, 4) shouldEqual Some(5)
  }

  "findNth" should "return None when invalid value is supplied as N" in {
    val list = List[Int](1, 2)
    ListUtil.findNth(list, 0) shouldEqual None
  }

  "findNth" should "return None when list has less elements than the value of N" in {
    val list = List[Int](1, 2)
    ListUtil.findNth(list, 3) shouldEqual None
  }

  "findNth" should "return None when provided with empty list" in {
    ListUtil.findNth(List[Int](), 0) shouldEqual None
  }

  "first" should "return first element of the list" in {
    val list = List[String]("10", "20")
    ListUtil.first(list) shouldEqual Some("10")
  }

  "first" should "return None for empty list" in {
    ListUtil.first(List()) shouldEqual None
  }

  "last" should "return the last element of the list" in {
    val list = List[Int](1, 2, 10, 5)
    ListUtil.last(list) shouldEqual Some(5)
  }

  "last" should "return None for empty list" in {
    ListUtil.last(List()) shouldEqual None
  }

  "count" should "return number of items in list" in {
    val list = List[String]("1", "2", "3", "4")
    val count = ListUtil.count(list)
    count shouldBe 4
  }

  "count" should "return 0 when empty list is passed" in {
    val count = ListUtil.count(List[Int]())
    count shouldBe 0
  }

  "reverse" should "reverse the elements in the list" in {
    val list = List[Int](1, 2, 3)
    val reversedList = ListUtil.reverse(list)
    reversedList shouldBe List[Int](3, 2, 1)
  }

  "reverse" should "return empty list when input list is empty" in {
    val reversedList = ListUtil.reverse(List[String]())
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
    val compressedList = ListUtil.compress(List[Int]())
    compressedList.length shouldBe 0
  }

  "dropNth" should "remove the Nth element from the list" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val modifiedList = ListUtil.dropNth(list, 2)
    modifiedList shouldBe List[Int](1, 3, 4, 5)
  }

  "dropNth" should "throw exception when n is greater than the number of elements in list" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val expectedException = intercept[RuntimeException] {
      ListUtil.dropNth(list, 6)
    }
    expectedException.getMessage shouldEqual "Invalid value of n"
  }

  "dropNth" should "throw exception when n is lesser than 1" in {
    val list = List[Int](1, 2, 3, 4, 5)
    val expectedException = intercept[RuntimeException] {
      ListUtil.dropNth(list, 0)
    }
    expectedException.getMessage shouldEqual "Invalid value of n"
  }

  "split" should "split the list into two at the position mentioned by n" in {
    val list = List[Int](1, 2, 3, 4, 5, 6)
    val (firstList, secondList) = ListUtil.split(list, 3)
    firstList shouldEqual List[Int](1, 2, 3)
    secondList shouldEqual List[Int](4, 5, 6)
  }

  "slice" should "slice the list between the two given position" in {
    val list = List[Int](1, 2, 3, 4, 5, 6)
    val result = ListUtil.slice(list, 1, 5)
    result shouldEqual List[Any](1, 2, 3, 4, 5)
  }
}
