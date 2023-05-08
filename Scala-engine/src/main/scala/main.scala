import java.awt.{Color, Font, Graphics}
import javax.swing.{JFrame, JPanel}
import scala.util.Random
object GameEngine {
  def main(args: Array[String]): Unit = {
    //abstractEngine(6, 7, 2, "connect4")
    abstractEngine(9, 9, 1, "sudoku")
  }

  def Connect4_drawer(state: Array[Array[Int]]): Unit = {
    println("1 2 3 4 5 6 7")
    state.map(row => {
      row.map {
        case 0 => " "
        case 1 => "R"
        case 2 => "Y"
      }.mkString("|")
    }).foreach(println)
    println()
  }


  def Connect4_controller(game: String, move: String, state: (Array[Array[Int]], Int)): (Boolean, Array[Array[Int]]) = {

    def updateState(player: Int, col: Int, state: Array[Array[Int]]): Option[(Array[Array[Int]], (Int, Int))] = {
      (5 to 0 by -1).find(row => state(row)(col) == 0).map { row =>
        (state.updated(row, state(row).updated(col, player)), (row, col))
      }
    }

    game match {
      case "connect4" =>
        val player = state._2 % 2 + 1
        val col = move.toUpperCase().charAt(0) - 'A' //
        //val col =  move.toInt - 'a'.toInt
        if (col < 0 || col > 6 || state._1(0)(col) != 0) {
          (false, state._1)
        } else {
          updateState(player, col, state._1) match {
            case Some((newState, (row, col))) => (true, newState)
            case None => (false, state._1)
          }
        }
    }
  }


  def abstractEngine(dimx: Int, dimy: Int, numOfPlayers: Int, game: String) =
  {
    val frame = new JFrame()
    var state = (Array.ofDim[Int](dimx, dimy), 0);
    game match
      case "connect4" =>
        Connect4_drawer(state(0))
        drawBoardGUI_Connect4(state(0), frame)
      case "sudoku" =>
        state = (fillRandom(state(0)),0) ///////
        Sudokudrawer(state(0))
        drawBoardGUI_Sudoku(state(0),frame)


    while(true) {
      val input = scala.io.StdIn.readLine()
      var currentState: (Boolean, Array[Array[Int]]) = null
      game match
        case "connect4" =>
          currentState = Connect4_controller(game, input, state)
        case "sudoku" =>
          currentState = Sudokucontroller(game, input, state)

      if(currentState(0) == true) {
        state = (currentState(1), (state(1)+1) % numOfPlayers)
        game match
          case "connect4" =>
            Connect4_drawer(state(0))
            drawBoardGUI_Connect4(state(0), frame)
          case "sudoku" =>
            Sudokudrawer(state(0))
            drawBoardGUI_Sudoku(state(0), frame)
      } else
        println("Invalid move!")
    }
  }

  def drawBoardGUI_Connect4(state: Array[Array[Int]], frame: JFrame): Unit = {
    val panel = new JPanel() {
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.setColor(Color.BLUE)
        g.fillRect(0, 0, getWidth, getHeight)

        def drawCircle(x: Int, y: Int, color: Color): Unit = {
          g.setColor(color)
          g.fillOval(x + 15, y + 15, getWidth / 7 - 30, getHeight / 6 - 30)
        }

        for {
          row <- state.indices
          col <- state(row).indices
        } {
          val x = col * getWidth / 7
          val y = row * getHeight / 6
          g.setColor(Color.WHITE)
          g.fillOval(x + 10, y + 10, getWidth / 7 - 20, getHeight / 6 - 20)
          state(row)(col) match {
            case 1 => drawCircle(x, y, Color.RED)
            case 2 => drawCircle(x, y, Color.YELLOW)
            case _ =>
          }
        }
      }
    }

    frame.add(panel)
    frame.setSize(700, 600)
    frame.setVisible(true)
  }
////////////////////////////////////////////////SUDUKU////////////////////////////////////////////
def isValidMoveSudoku(board: Array[Array[Int]], row: Int, col: Int, num: Int): Boolean = {
  val boxRow = 3 * (row / 3)
  val boxCol = 3 * (col / 3)
  row >= 0 && row < 9 && col >= 0 && col < 9 && num >= 1 && num <= 9 && !(0 until 9).exists { i =>
    board(row)(i) == num ||
      board(i)(col) == num ||
      board(boxRow + (i % 3))(boxCol + (i / 3)) == num
  }
}

  import scala.annotation.tailrec
  import scala.util.Random

  def fillRandom(array: Array[Array[Int]]): Array[Array[Int]] = {

    def fillRandomHelper(count: Int, acc: Array[Array[Int]]): Array[Array[Int]] = {
      if (count >= 17) acc
      else {
        val random = new Random()
        val row = random.nextInt(9)
        val col = random.nextInt(9)
        if (acc(row)(col) == 0) {
          val value = Stream.continually(random.nextInt(9) + 1).find(isValidMoveSudoku(acc, row, col, _)).get
          val updatedArray = acc.updated(row, acc(row).updated(col, value))
          fillRandomHelper(count + 1, updatedArray)
        } else {
          fillRandomHelper(count, acc)
        }
      }
    }

    fillRandomHelper(0, array)
  }

  def Sudokucontroller(game: String, move: String, state: (Array[Array[Int]], Int)): (Boolean, Array[Array[Int]]) = {

    val indexedMove = move.split(" ").map(_.toInt)       //toUpperCase().charAt(0) - 'A'
    indexedMove match {
      case Array(row, col, value) if  isValidMoveSudoku(state._1, row, col, value) =>
        val newBoard = state._1.updated(row, state._1(row).updated(col, value))
        (true, newBoard)
      case _ =>
        (false, state._1)
    }
  }


  def Sudokudrawer(board: Array[Array[Int]]): Unit = {
    // Draw the Sudoku board
    println("   0 1 2   3 4 5   6 7 8 ")
    println("  +-------+-------+-------+")
    (0 until 9).foreach { i =>
      if (i % 3 == 0) {
        println("  |       |       |       |")
      }
      print(i + " ")
      (0 until 9).foreach { j =>
        if (j % 3 == 0) {
          print("| ")
        }
        val cell = board(i)(j)
        val value = if (cell == 0) " " else cell.toString
        if ((i == 4) && (j == 4)) {
          print(value + " ")
        } else if (cell == 0) {
          print(value + " ")
        } else {
          print(value + " ")
        }
      }
      print("|")
      println()
    }

    println("  +-------+-------+-------+")
  }


 
///////////
  def drawBoardGUI_Sudoku(board: Array[Array[Int]], frame: JFrame): Unit = {
    val panel = new JPanel() {
      override def paintComponent(g: Graphics): Unit = {
        super.paintComponent(g)
        g.setColor(Color.BLACK)

        // Draw grid lines
        (0 to 9).foreach { i =>
          val x = i * getWidth / 9
          g.drawLine(x, 0, x, getHeight)
          val y = i * getHeight / 9
          g.drawLine(0, y, getWidth, y)
        }

        // Draw numbers
        g.setColor(Color.BLUE)
        board.indices.foreach { i =>
          board(i).indices.foreach { j =>
            val x = j * getWidth / 9 + getWidth / 20
            val y = i * getHeight / 9 + getHeight / 12
            val value = board(i)(j)
            if (value != 0) {
              g.drawString(value.toString, x, y)
            }
          }
        }
      }
    }

    frame.add(panel)
    frame.setSize(500, 500)
    frame.setVisible(true)
  }





  // Start the game




}

