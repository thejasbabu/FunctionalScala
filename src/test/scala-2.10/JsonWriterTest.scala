import org.scalatest.{FlatSpec, Matchers}

class JsonWriterTest extends FlatSpec with Matchers {
  "JsonWriter" should "serialize Boolean value" in {
    val jsonValue = JsonBool(false)
    JsonWriter.toJson(jsonValue) shouldEqual "false"
  }

  "JsonWriter" should "serialize Null value" in {
    val jsonValue = JsonNull()
    JsonWriter.toJson(jsonValue) shouldEqual "null"
  }

  "JsonWriter" should "serialize Int value" in {
    val jsonValue = JsonInt(10)
    JsonWriter.toJson(jsonValue) shouldEqual "10"
  }

  "JsonWriter" should "serialize Double value" in {
    val jsonValue = JsonDouble(10.02)
    JsonWriter.toJson(jsonValue) shouldEqual "10.02"
  }

  "JsonWriter" should "serialize String value" in {
    val jsonValue = JsonString("blah")
    JsonWriter.toJson(jsonValue) shouldEqual "\"blah\""
  }

  "JsonWriter" should "serialize Array Jsonvalue" in {
    val elements = Array[JsonValue](JsonInt(10), JsonBool(true), JsonString("hello"))
    val jsonValue = JsonArray(elements)
    JsonWriter.toJson(jsonValue) shouldEqual "[10,true,\"hello\"]"
  }

  "JsonWriter" should "serialize Map Jsonvalue" in {
    val elements = Map[String, JsonValue](
      "one" -> JsonArray(Array[JsonValue](JsonInt(10), JsonNull(), JsonString("hello"))),
      "two" -> JsonString("three")
    )

    val jsonValue = JsonMap(elements)
    JsonWriter.toJson(jsonValue) shouldEqual "{\"one\":[10,null,\"hello\"],\"two\":\"three\"}"
  }
}
