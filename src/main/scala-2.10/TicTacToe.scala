object TicTacToe {
  val horizontal: PartialFunction[Set[Int], Boolean] = {
      case x if x.equals(Set(1, 2, 3)) => true
      case x if x.equals(Set(4, 5, 6)) => true
      case x if x.equals(Set(7, 8, 9)) => true
  }

  val vertical: PartialFunction[Set[Int], Boolean] = {
      case x if x.equals(Set(1, 4, 7)) => true
      case x if x.equals(Set(2, 5, 8)) => true
      case x if x.equals(Set(3, 6, 9)) => true
  }

  val diagonal: PartialFunction[Set[Int], Boolean] = {
      case x if x.equals(Set(1, 5, 9)) => true
      case x if x.equals(Set(3, 5, 7)) => true
  }

  def check(moves: Set[Int]): Boolean = {
    val winningMoves = horizontal orElse vertical orElse diagonal
    winningMoves.isDefinedAt(moves)
  }
}
