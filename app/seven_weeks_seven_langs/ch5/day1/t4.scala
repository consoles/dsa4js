// 编写一个游戏，可以用X、O和空字符玩井字游戏（tic-tac-toe），
// 检查是否有胜者，或是否不分胜负，或目前没有胜者。适当地使用类。

// 3 * 3 的棋盘
class Board(matrix: Array[Array[String]]) {
  private val board = Array.ofDim[String](3, 3)

  for (i <- 0 until 3) {
    for (j <- 0 until 3) {
      board(i)(j) = matrix(i)(j)
    }
  }

  def get(row: Int, col: Int): String = {
    board(row)(col)
  }

  def set(row: Int, col: Int, marker: String): Unit = {
    board(row)(col) = marker
  }

  override def toString: String = {
    board.map(_.mkString("|")).mkString("\n")
  }
}

// https://zhuanlan.zhihu.com/p/172479203
object Game {
  // 如果游戏存在获胜者，就返回该游戏的获胜者使用的字符（"X"或"O"）；如果游戏以平局结束，则返回 "Draw"；如果仍会有行动（游戏未结束），则返回 "Pending"
  def checkWinner(board: Board): String = {
    val size = 3
    // 横
    for (i <- 0 until size) {
      if (board.get(i, 0) == board.get(i, 1) && board.get(i, 0) == board.get(i, 2)) {
        return board.get(i, 0)
      }
    }
    // 纵
    for (i <- 0 until size) {
      if (board.get(0, i) == board.get(1, i) && board.get(0, i) == board.get(2, i)) {
        return board.get(0, i)
      }
    }
    // 对角
    if (board.get(0, 0) == board.get(1, 1) && board.get(0, 0) == board.get(2, 2)) {
      return board.get(0, 0)
    }
    // 反对角
    if (board.get(0, 2) == board.get(1, 1) && board.get(0, 2) == board.get(2, 0)) {
      return board.get(0, 2)
    }
    // 检查是否存在空位
    var isEmpty = false
    for (i <- 0 until size) {
      for (j <- 0 until size) {
        if (board.get(i, j) == " ") {
          isEmpty = true
        }
      }
    }
    if (isEmpty) {
      "Pending"
    } else {
      "Draw"
    }
  }

  def main(args: Array[String]): Unit = {
    var board = new Board(
      Array(
        Array("O", " ", "X"),
        Array(" ", "X", "O"),
        Array("X", " ", "O")
      )
    )
    println(board.toString + " => " + checkWinner(board))
    board = new Board(
      Array(
        Array("O", "O", "X"),
        Array("X", "X", "O"),
        Array("O", "X", "O")
      )
    )
    println(board.toString + " => " + checkWinner(board))
    board = new Board(
      Array(
        Array("O", "O", "X"),
        Array("X", "X", "O"),
        Array("O", "X", " ")
      )
    )
    println(board.toString + " => " + checkWinner(board))
  }
}
