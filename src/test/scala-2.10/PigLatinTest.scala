import org.scalatest.{FlatSpec, Matchers}

class PigLatinTest extends FlatSpec with Matchers{
  "PigLatin" should "convert words that begin with consonant sounds to pig latin format" in {
    PigLatin.convert("banana") shouldEqual("ananabay")
    PigLatin.convert("say") shouldEqual("aysay")
    PigLatin.convert("too") shouldEqual("ootay")
    PigLatin.convert("bagel") shouldEqual("agelbay")
  }

  "PigLatin" should "return the words that doesn't have any vowels" in {
    PigLatin.convert("tv") shouldEqual("tv")
  }
}
