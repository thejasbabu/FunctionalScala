// https://en.wikipedia.org/wiki/Pig_Latin for rules
object PigLatin {
  val vowels = Set('a', 'e', 'i', 'o', 'u')

  val isVowel: Char => Boolean = (c: Char) => {
    vowels.contains(c.toLower)
  }

  def spiltAtVowel(str: String): (String, String) = {
    str.splitAt(str.indexWhere(isVowel))
  }

  def convert(word: String): String = {
    spiltAtVowel(word) match {
      case ("", tail) => tail
      case (head, tail) => tail + head + "ay"
    }
  }
}
