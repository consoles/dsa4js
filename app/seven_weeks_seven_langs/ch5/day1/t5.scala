// 加分题：让两个选手玩井字游戏。

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
        if (board.get(i, 0) != " " && board.get(i, 0) == board.get(i, 1) && board.get(i, 0) == board.get(i, 2)) {
            return board.get(i, 0)
        }
        }
        // 纵
        for (i <- 0 until size) {
        if (board.get(0, i) != " " && board.get(0, i) == board.get(1, i) && board.get(0, i) == board.get(2, i)) {
            return board.get(0, i)
        }
        }
        // 对角
        if (board.get(0, 0) != " " && board.get(0, 0) == board.get(1, 1) && board.get(0, 0) == board.get(2, 2)) {
        return board.get(0, 0)
        }
        // 反对角
        if (board.get(0, 2) != " " && board.get(0, 2) == board.get(1, 1) && board.get(0, 2) == board.get(2, 0)) {
        return board.get(0, 2)
        }
        // 检查是否存在空位
        for (i <- 0 until size) {
        for (j <- 0 until size) {
            if (board.get(i, j) == " ") {
            return "Pending"
            }
        }
        }
        "Draw"
    }

    def main(args: Array[String]): Unit = {
        val board = new Board(
            Array(
                Array(" ", " ", " "),
                Array(" ", " ", " "),
                Array(" ", " ", " ")
            )
        )
        println("init empty board:" + board.toString)
        var isPlayerX = false
        while (true) {
            val result = checkWinner(board)
            if (result == "Pending") {
                println(board.toString)
                val player = if (isPlayerX) "X" else "O"
                println(s"玩家 $player 落子(x,y):") 
                val input = scala.io.StdIn.readLine() 
                val Array(x, y) = input.split(",").map(_.trim.toInt)
                if (x < 0 || x > 2 || y < 0 || y > 2) { 
                    println("无效的输入，请输入 0-2 之间的数字") 
                } else if (board.get(x, y) != " ") {
                    println("该位置已经落子了，请选择其他位置") 
                } else { 
                    board.set(x, y, player) 
                    isPlayerX = !isPlayerX 
                } 
            } else {
                println(board.toString)
                println(s"游戏结束，结果为：$result") 
                return 
            }
        }
    }
}
