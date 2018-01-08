object TicTacToe {
  def horizontal(moves: Set[Int]): Boolean = {
    moves match {
      case x if x.equals(Set(1, 2, 3)) => true
      case x if x.equals(Set(4, 5, 6)) => true
      case x if x.equals(Set(7, 8, 9)) => true
      case _ => false
    }
  }
}
