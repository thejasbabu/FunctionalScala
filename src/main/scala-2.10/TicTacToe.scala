object TicTacToe {
  val horizontal: PartialFunction[Set[Int], Boolean] = {
      case x if Set(1, 2, 3).subsetOf(x) => true
      case x if Set(4, 5, 6).subsetOf(x) => true
      case x if Set(7, 8, 9).subsetOf(x) => true
  }

  val vertical: PartialFunction[Set[Int], Boolean] = {
      case x if Set(1, 4, 7).subsetOf(x) => true
      case x if Set(2, 5, 8).subsetOf(x) => true
      case x if Set(3, 6, 9).subsetOf(x) => true
  }

  val diagonal: PartialFunction[Set[Int], Boolean] = {
      case x if Set(1, 5, 9).subsetOf(x) => true
      case x if Set(3, 5, 7).subsetOf(x) => true
  }

  def check(moves: Set[Int]): Boolean = {
    val winningMoves = horizontal orElse vertical orElse diagonal
    winningMoves.isDefinedAt(moves)
  }
}
