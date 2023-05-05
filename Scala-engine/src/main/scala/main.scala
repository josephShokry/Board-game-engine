
@main
def main(): Unit = {
  println("Hello world!")
  abstractEngine(8, 8, 2, "checkers", checkersController, checkersDrawer, initializeCheckersBoard)
}


def abstractEngine(dimx: Int, dimy: Int, numOfPlayers: Int, game: String,
                   controller: (String, (Array[Array[String]], Int)) => (Boolean, Array[Array[String]]),
                   drawer: Array[Array[String]] => Unit,
                   initBoard: () => Array[Array[String]]) =
{
  var state = (initBoard(), 0);
  drawer(state(0))

  while(true) {
    val input = scala.io.StdIn.readLine()
    val currentState: (Boolean, Array[Array[String]]) = controller(input, state)
    if(currentState(0) == true) {
      state = (currentState(1), (state(1)+1) % numOfPlayers)
      drawer(state(0))
    } else
      println("Invalid move!")
  }
}

def checkersController(move: String, state: (Array[Array[String]], Int)): (Boolean, Array[Array[String]]) =
{
  val indexedMove = changeLettersToIndex(move)
  val board: Array[Array[String]] = state(0)
  val currentPlayer = state(1)

  if(indexedMove.length != 2)
      return (false, state(0))
  val cell1 = Array(8 - indexedMove(0)(1), indexedMove(0)(0))

  val cell2 = Array(8 - indexedMove(1)(1), indexedMove(1)(0))

  if(board(cell1(0))(cell1(1)) == null || board(cell2(0))(cell2(1)) != null ||
    !checkIfCurrentPlayerCanMovePiece(board(cell1(0))(cell1(1)), state(1)) ||
    !isValid(8, cell1(0), cell1(1)) || !isValid(8, cell2(0), cell2(1)))
    return (false, state(0))

  if((cell1(0) - 1 == cell2(0) && cell1(1) + 1 == cell2(1) || cell1(0) - 1 == cell2(0) && cell1(1) - 1 == cell2(1)) && currentPlayer == 0)
    updateAndRemove(board, cell1, cell2)

  else if(currentPlayer == 1 && (cell1(0) + 1 == cell2(0) && cell1(1)-1 == cell2(1) || cell1(0) + 1 == cell2(0) && cell1(1) + 1 == cell2(1)))
    updateAndRemove(board, cell1, cell2)

  else if(currentPlayer == 1 && cell1(0) + 2 == cell2(0) && cell1(1) + 2 == cell2(1) && board(cell1(0)+1)(cell1(1)+1) == "red"){
    board(cell1(0)+1)(cell1(1)+1) = null
    updateAndRemove(board, cell1, cell2)
  }else if(currentPlayer == 1 && cell1(0) + 2 == cell2(0) && cell1(1) - 2 == cell2(1) && board(cell1(0)+1)(cell1(1)-1) == "red") {
    board(cell1(0)+1)(cell1(1)-1) = null
    updateAndRemove(board, cell1, cell2)
  }else if (currentPlayer == 0 && cell1(0) - 2 == cell2(0) && cell1(1) - 2 == cell2(1) && board(cell1(0)-1)(cell1(1)-1) == "black"){
    board(cell1(0)-1)(cell1(1)-1) = null
    updateAndRemove(board, cell1, cell2)
  }else if(currentPlayer == 0 && cell1(0) - 2 == cell2(0) && cell1(1) + 2 == cell2(1) && board(cell1(0)-1)(cell1(1)+1) == "black") {
    board(cell1(0)-1)(cell1(1)+1) = null
    updateAndRemove(board, cell1, cell2)
  }else
    return (false, state(0))

  (true, board)
}

def updateAndRemove(board: Array[Array[String]], cell1: IndexedSeq[Int], cell2: IndexedSeq[Int]) = {
  board(cell2(0))(cell2(1)) = board(cell1(0))(cell1(1))
  board(cell1(0))(cell1(1)) = null
}

def isValid(n: Int, x: Int, y: Int) = x >= 0 && x < n && y >= 0 && y < n

def changeLettersToIndex = (move: String) =>
  move.split(' ').map(arr => arr.map(c => if(c.isLetter) c.toInt - 'a'.toInt  else c.toInt - '0'.toInt));

def checkIfCurrentPlayerCanMovePiece = (boardElem: String, currentPlayer: Int) =>
  if(currentPlayer == 0 && boardElem == "red"|| currentPlayer == 1 && boardElem == "black")
     true
  else
    false

def initializeCheckersBoard()= {
  val board = Array.ofDim[String](8, 8)
  var idx = 1;
  while(idx < 8)
  {
    board(0)(idx) = "black"
    board(1)(idx - 1) = "black"
    board(2)(idx) = "black"

    board(5)(idx - 1) = "red"
    board(6)(idx) = "red"
    board(7)(idx - 1) = "red"
    idx = idx + 2
  }
  board
}

def checkersDrawer(board: Array[Array[String]]): Unit = {
  var idx = 0;
  while(idx < 8)
  {
    var idx2 = 0;
    while(idx2 < 8)
    {
      if(board(idx)(idx2) == null)
        print("0 ")
      else
        print(board(idx)(idx2) + " ")
      idx2 = idx2 + 1
    }
    println()
    idx = idx + 1
  }
}


enum e:
  case red, black

def drawer(drawer: Unit): Unit = {
  println("drawer")
}

def controller(game: String, move: String, state: (Array[Array[Int]], Int)): (Boolean, Array[Array[Int]]) = {
  // change letters to digits
  val indexedMove = move.split(' ').map(arr => arr.map(c => if(c.isLetter) c.toInt - 'a'.toInt else c))
  println(indexedMove)
  game match
    case "chess" => println("chess controller called")
    case _ => println("not a valid game")

  (true, Array.ofDim(8, 8))
}