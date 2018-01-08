import org.scalatest.{FlatSpec, Matchers}

class TicTacToeTest extends FlatSpec with Matchers {
  "horizontal" should "return true when horizontal positions are selected" in {
    TicTacToe.horizontal(Set(1, 2, 3)) shouldEqual true
  }

  "horizontal" should "return false when set does not denote horizontal positions" in {
    TicTacToe.horizontal(Set(1, 5, 3)) shouldEqual false
  }
}
