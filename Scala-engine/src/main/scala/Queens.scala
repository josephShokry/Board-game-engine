package Queens

import Chess.changeLettersToIndex
import org.jpl7.Query

import java.awt.{Color, Graphics2D, Image, RenderingHints, Toolkit}
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import scala.swing.{BorderPanel, Dimension, GridPanel, Label, MainFrame}
def EQueensController(move: String, state: (Array[Array[Char]], Int)): (Boolean, Array[Array[Char]]) =
{
  if(move.equals("solve"))
    return (true, solveEQueens(state(0)))

  val indexedMove = changeLettersToIndex(move)
  if(indexedMove.length != 1)
    return (false, state(0))
  val cell = indexedMove(0)
  val x = 8 - cell(1); var y = cell(0);
  val board:Array[Array[Char]] = state(0);

  if (x < 0 || y < 0 || x >= 8 || y >= 8)
    return (false, state(0));

  if (board(x)(y) == 'Q')
  {
    board(x)(y) = '0';
    return (true, board);
  }

  def checkQueenCellAttacked(x2:Int, y2:Int) : (Boolean) =
  {
    if (x == x2 && y == y2 || board(x2)(y2) == '0') return false;
    if (x == x2 || y == y2 || x - y == x2 - y2 || x + y == x2 + y2)
      return true;
    false;
  }

  val numOfAttackedQueens = board.zipWithIndex.flatMap { case (row, rowIndex) => row.zipWithIndex.map {
    case (col, colIndex) => checkQueenCellAttacked(rowIndex, colIndex) }
  }.count((x: Boolean) => x)

  if (numOfAttackedQueens > 0)
    return (false, board);

  board(x)(y) = 'Q';

  (true, board)
}
def EQueensDrawer(board: Array[Array[Char]]): Unit = {
  var idx = 0;
  while(idx < 8)
  {
    var idx2 = 0;
    while(idx2 < 8)
    {
      print(board(idx)(idx2) + " ")
      idx2 = idx2 + 1
    }
    println()
    idx = idx + 1
  }
}

def initializeEQueenBoard() = {
  val board = Array.ofDim[Char](8, 8)
  var idx = 0;
  while(idx < 8)
  {
    var idx2 = 0;
    while(idx2 < 8)
    {
      board(idx)(idx2) = '0'
      idx2 = idx2 + 1
    }
    idx = idx + 1
  }
  board
}

def solveEQueens(state: (Array[Array[Char]])): (Array[Array[Char]]) = {
  val prologFile = new Query("consult('src/main/prolog/EQueensSolver.pl')")
  if(!prologFile.hasSolution){
    println("prolog file not found")
    return state
  }
  val prologString = convertToPrologFormat(state)
  val query = s"Qs = $prologString, n_queens(8, Qs), label(Qs)."
  val prologResult = Query(query)
  if(!prologResult.hasSolution) {
    println("Can't find solution")
    return state
  }

  println("Solution found!")
  val solution = prologResult.oneSolution().get("Qs")
  updateBoardWithSolution(state, solution.toString)
}
def convertToPrologFormat(board: (Array[Array[Char]])): String = {
  var z = Array(-1,-1,-1,-1,-1,-1,-1,-1)
  board.zipWithIndex.foreach{
    case (row, rowIndex) => row.zipWithIndex.foreach{
    case (element, colIndex) => if(element == 'Q') z(colIndex) = rowIndex+1}}
 var s = "["
  z.foreach(index => if(index == -1)s = s + "_, "
  else s = s + s"$index, ")
  s = s.substring(0, s.length - 2)
  s = s + "]"
  s
}

def updateBoardWithSolution(state: Array[Array[Char]], inputString: String): Array[Array[Char]] = {
  val cleanString = inputString.replace("[", "")
    .replace("]", "").replace(" ", "")
  val array = cleanString.split(",")
  array.zipWithIndex.foreach{case (rowIndex,colIndex) => state(rowIndex.toInt - 1)(colIndex) = 'Q'}

  state
}

def getPieceImage(): Image = {
//  val colorString = if (isWhite) "white" else "black"
  val img = ImageIO.read(new File(s"src/main/scala/pieces/black/queen.png"))
  val resizedImg = img.getScaledInstance(64, 64, Image.SCALE_SMOOTH)
  val bImg = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
  val g2d = bImg.createGraphics()
  g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g2d.drawImage(resizedImg, 0, 0, null)
  g2d.dispose()
  Toolkit.getDefaultToolkit().createImage(bImg.getSource())
}
def drawGUIEQueen(board: Array[Array[Char]]): Unit = {
  val darkSquare = new Color(209, 139, 71)
  val lightSquare = new Color(255, 206, 158)

  val letters = Array("A", "B", "C", "D", "E", "F", "G", "H")
  val numbers = Array("1", "2", "3", "4", "5", "6", "7", "8")

  val boardGUI = new GridPanel(8, 8) {
    preferredSize = new Dimension(512, 512)
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      val pieceSize = 64
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          val squareColor = if ((i + j) % 2 == 0) lightSquare else darkSquare
          g.setColor(squareColor)
          g.fillRect(j * pieceSize, i * pieceSize, pieceSize, pieceSize)
              if(board(i)(j)!='0'){
                g.drawImage(getPath(i, j, board), j * pieceSize, i * pieceSize, pieceSize, pieceSize, null)
              }
        }
      }
    }
  }
  val rowLabels = new GridPanel(8, 1) {
    preferredSize = new Dimension(17, 512)
    for (i <- 0 until 8) {
      contents += new Label(numbers(7 - i))
    }
  }
  val colLabels = new GridPanel(1, 8) {
    preferredSize = new Dimension(512, 17)
    for (j <- 0 until 8) {
      contents += new Label(letters(j))
    }
  }

  val frame = new MainFrame {
    title = "Chess Board"
    contents = new BorderPanel {
      add(boardGUI, BorderPanel.Position.Center)
      add(rowLabels, BorderPanel.Position.West)
      add(colLabels, BorderPanel.Position.South)
    }
    pack()
    centerOnScreen()
    open()
  }
}
def getPath(i: Int, j: Int, board: Array[Array[Char]]):Image = board(i)(j) match{
  case 'Q' => getPieceImage()
  case _ => null
}