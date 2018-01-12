import org.scalatest.{FlatSpec, Matchers}

class TicTacToeTest extends FlatSpec with Matchers {
  "horizontal" should "return true when horizontal positions are selected" in {
    TicTacToe.horizontal.isDefinedAt(Set(1, 2, 5, 3)) shouldEqual true
  }

  "horizontal" should "return false when set does not denote horizontal positions" in {
    TicTacToe.horizontal.isDefinedAt(Set(1, 4, 5, 3)) shouldEqual false
  }

  "vertical" should "return true when vertical positions are selected" in {
    TicTacToe.vertical.isDefinedAt(Set(2, 4, 1, 7)) shouldEqual true
  }

  "vertical" should "return false when set does not denote vertical positions" in {
    TicTacToe.vertical.isDefinedAt(Set(7, 5, 3, 1)) shouldEqual false
  }

  "diagonal" should "return true when diagonal positions are selected" in {
    TicTacToe.diagonal.isDefinedAt(Set(9, 5, 1)) shouldEqual true
  }

  "diagonal" should "return false when set does not denote diagonal positions" in {
    TicTacToe.diagonal.isDefinedAt(Set(1, 2, 3)) shouldEqual false
  }

  "check" should "check if the moves can win/lose a game in tic-tac-toe" in {
    TicTacToe.check(Set(1, 2, 4, 3)) shouldEqual true
    TicTacToe.check(Set(1, 2, 4, 7)) shouldEqual true
    TicTacToe.check(Set(3, 9, 5, 1)) shouldEqual true
    TicTacToe.check(Set(1, 5, 4, 3)) shouldEqual false
  }
}
