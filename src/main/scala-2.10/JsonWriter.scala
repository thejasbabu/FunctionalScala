sealed trait JsonValue
case class JsonNull() extends JsonValue
case class JsonBool(value: Boolean) extends JsonValue
case class JsonInt(value: Int) extends JsonValue
case class JsonDouble(value: Double) extends JsonValue
case class JsonString(value: String) extends JsonValue
case class JsonArray(value: Array[JsonValue]) extends JsonValue
case class JsonMap(value: Map[String, JsonValue]) extends JsonValue


object JsonWriter {
    def toJson(value: JsonValue): String =
      value match {
        case JsonNull() => "null"
        case JsonBool(x) => x.toString
        case JsonInt(x) => x.toString
        case JsonDouble(x) => x.toString
        case JsonString(x) =>  "\"" + x.toString + "\""
        case JsonArray(x) => "[" + x.map(toJson).mkString(",") + "]"
        case JsonMap(x) => "{" + x.map(e => "\"" + e._1.toString + "\":" + toJson(e._2)).mkString(",") + "}"
      }

}
