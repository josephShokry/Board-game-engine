import Colors.{Colors, White}
import Pieces.{Empty, Pieces}

import javax.swing.ImageIcon
import scala.swing.{Component, MainFrame, SimpleSwingApplication}
import scala.util.control.Breaks.break

@main
def main(): Unit = {
  println("Hello world!")
  abstractEngine(8, 8, 2, "chess",chessController,chessDrawer)
}

def abstractEngine(dimx: Int, dimy: Int, numOfPlayers: Int, game: String,
                   controller: ((String,(Array[Array[(Colors,Pieces)]], Int)))=> (Boolean, Array[Array[(Colors,Pieces)]]),
                   drawer: Array[Array[(Colors,Pieces)]] => Unit) = {
  var state = (initBoard(dimx, dimy),0)
  chessDrawer(state(0))
  while(true) {
    var input = scala.io.StdIn.readLine()
    var currentState: (Boolean, Array[Array[(Colors,Pieces)]]) = controller(input, state)
    if(currentState(0) == true) {
      state = (currentState(1), (state(1)+1) % numOfPlayers)
      var playerturn = state(1)
      println(s"the player turn is $playerturn")
      chessDrawer(state(0))
//      drawChessBoardWithPieces(state(0))
    } else
      println("Invalid move!")
  }
}


////////////////////////////////////////////////////////////////////////////////////////////////
object Pieces extends Enumeration {
  type Pieces = Value

  val Pown = Value("Pown")
  val Knight = Value("Knight")
  val Bishop = Value("Bishop")
  val Rook = Value("Rook")
  val King = Value("King")
  val Queen = Value("Queen")
  val Empty = Value("Empty")
}
object Colors extends Enumeration {
  type Colors = Value

  val White = Value("White")
  val Black = Value("Black")
  val Empty = Value("Empty")
}

def initBoard(dimx: Int, dimy: Int):Array[Array[(Colors,Pieces)]]={
  var state = Array.ofDim[(Colors,Pieces)](dimx, dimy)
  (0 until dimx).foreach { row =>
    (0 until dimy).foreach { col =>
      state(row)(col) = (Colors.Empty,Pieces.Empty)
    }
  }
  state(0)(0) = (Colors.White,Pieces.Rook)
  state(0)(1) = (Colors.White,Pieces.Knight)
  state(0)(2) = (Colors.White,Pieces.Bishop)
  state(0)(3) = (Colors.White,Pieces.King)
  state(0)(4) = (Colors.White,Pieces.Queen)
  state(0)(5) = (Colors.White,Pieces.Bishop)
  state(0)(6) = (Colors.White,Pieces.Knight)
  state(0)(7) = (Colors.White,Pieces.Rook)

  state(7)(0) = (Colors.Black,Pieces.Rook)
  state(7)(1) = (Colors.Black,Pieces.Knight)
  state(7)(2) = (Colors.Black,Pieces.Bishop)
  state(7)(3) = (Colors.Black,Pieces.King)
  state(7)(4) = (Colors.Black,Pieces.Queen)
  state(7)(5) = (Colors.Black,Pieces.Bishop)
  state(7)(6) = (Colors.Black,Pieces.Knight)
  state(7)(7) = (Colors.Black,Pieces.Rook)

  state
}

def chessController(state:(String,(Array[Array[(Colors,Pieces)]], Int))) : (Boolean, Array[Array[(Colors,Pieces)]]) = {
  var moveFrom = (state(0).take(1).toInt, state(0).drop(1).take(1).toInt)
  var moveTo = (state(0).drop(3).take(1).toInt, state(0).drop(4).take(1).toInt)

  var validatingMove = validate(state(1)(0), moveTo, moveFrom, state(1)(1))

  if(validatingMove) applyMove(state(1)(0), moveTo, moveFrom)
  (validatingMove,state(1)(0))
}

def getValidMove(board:Array[Array[(Colors,Pieces)]], moveFrom:(Int,Int)):List[(Int,Int)]={
  matchPieces(board(moveFrom(0))(moveFrom(1))(1),board,moveFrom)
}
def matchPieces(piece:Pieces,board:Array[Array[(Colors,Pieces)]],
                moveFrom:(Int,Int)):List[(Int,Int)] = piece match{
  case Pieces.Queen => return addMovesStraigt(moveFrom,board)++addMovesDiagonal(moveFrom,board)
  case Pieces.King => return addKingMoves(moveFrom,board)
  case Pieces.Rook => return addMovesStraigt(moveFrom,board)
  case Pieces.Bishop => return addMovesDiagonal(moveFrom,board)
  case Pieces.Knight => return addKnightMoves(moveFrom,board)
  case Pieces.Pown => return List.empty[(Int,Int)].appended((1,1))
  case Pieces.Empty => return List.empty[(Int,Int)]
}
///////////////////////////////////////////////////////////////
def addMovesInConsecutiveCellsChess(inc1:Int, inc2:Int, range:Int, point:(Int,Int),
                                    board:Array[Array[(Colors,Pieces)]]): List[(Int,Int)] = {
  (1 to range).filter(i => checkValidPoint((point._1 + i * inc1, point._2 + i * inc2)) &&
    board(point._1 + i * inc1)(point._2 + i * inc2)._1 != board(point._1)(point._2)._1)
    .map(i => (point._1 + i * inc1, point._2 + i * inc2)).toList
}
def addMovesStraigt(point:(Int,Int),board:Array[Array[(Colors,Pieces)]]):List[(Int,Int)] ={
  val dims = List((1,0), (-1,0), (0,1), (0,-1));
  dims.flatMap(pair => addMovesInConsecutiveCellsChess(pair(0), pair(1), 8,point,board))
}

def addMovesDiagonal(point:(Int,Int),board:Array[Array[(Colors,Pieces)]]):List[(Int,Int)] ={
  val dims = List((1,1),(1,-1),(-1,1),(-1,-1))
  dims.flatMap(pair => addMovesInConsecutiveCellsChess(pair(0), pair(1), 8,point,board))
}

def addKingMoves(point:(Int,Int),board:Array[Array[(Colors,Pieces)]]):List[(Int,Int)] ={
  val dims = List((1,-1),(1,0),(1,1),(-1,-1),(-1,0),(-1,1),(0,-1),(0,1))
  dims.flatMap(pair => addMovesInConsecutiveCellsChess(pair(0), pair(1), 2,point,board))
}

def addKnightMoves(point:(Int,Int),board:Array[Array[(Colors,Pieces)]]):List[(Int,Int)] ={
  val dims = List((1,2),(1,-2),(-1,2),(-1,-2),(2,1),(2,-1),(-2,1),(-2,-1))
  dims.flatMap(pair => addMovesInConsecutiveCellsChess(pair(0), pair(1), 2,point,board))
}
def checkValidPoint(point:(Int,Int)):Boolean ={
  !(point(0).min(point(1)) < 0 || point(0).max(point(1)) >= 8)
}
///////////////////////////////////////////////////////////////



def validate(board:Array[Array[(Colors,Pieces)]], moveTo:(Int,Int), moveFrom:(Int,Int), turn:Int): Boolean ={
  if(board(moveFrom(0))(moveFrom(1))(0) != Colors.apply(turn)) return false
  getValidMove(board, moveFrom).contains(moveTo)
}


def applyMove(board:Array[Array[(Colors,Pieces)]], moveTo:(Int,Int), moveFrom:(Int,Int)): Unit ={
  board(moveTo(0))(moveTo(1)) = board(moveFrom(0))(moveFrom(1))
  board(moveFrom(0))(moveFrom(1)) = (Colors.Empty,Pieces.Empty)
}

def chessDrawer(board: Array[Array[(Colors,Pieces)]]) : Unit = {
  println("  " + (0 until 8).mkString("   "))
  (0 until 8).foreach { row =>
    (0 until 8).foreach { col => (row+col)%2 match
      case 0 => print(Console.CYAN_B + getAscii(row,col,board) + Console.RESET)
      case 1 => print(Console.BLACK_B + getAscii(row,col,board) + Console.RESET)
    }
    println("  " + row)
  }
  println
}

def getAscii(i: Int, j: Int, board: Array[Array[(Colors,Pieces)]]):String = board(i)(j) match{
  case (Colors.White, Pieces.Rook) =>return Console.RED + " ♜ " + Console.RESET
  case (Colors.White, Pieces.Knight) =>return Console.RED + " ♞ " + Console.RESET
  case (Colors.White, Pieces.Bishop) =>return Console.RED + " ♝ " + Console.RESET
  case (Colors.White, Pieces.Queen) =>return Console.RED + " ♛ " + Console.RESET
  case (Colors.White, Pieces.King) =>return Console.RED + " ♚ " + Console.RESET
  case (Colors.Black, Pieces.Rook) =>return Console.WHITE + " ♖ " + Console.RESET
  case (Colors.Black, Pieces.Knight) =>return Console.WHITE + " ♘ " + Console.RESET
  case (Colors.Black, Pieces.Bishop) =>return Console.WHITE + " ♗ " + Console.RESET
  case (Colors.Black, Pieces.Queen) =>return Console.WHITE + " ♕ " + Console.RESET
  case (Colors.Black, Pieces.King) =>return Console.WHITE + " ♔ " + Console.RESET
  case _ => "    "
  //        case (1, _) => Console.RED + "♟" + Console.RESET
  //        case (6, _) => Console.BLUE + "♙" + Console.RESET
}

/*
import scala.swing._
import java.awt.{Color, Graphics2D, Image, RenderingHints, Toolkit}
import javax.imageio.ImageIO
import java.io.File
import java.awt.image.BufferedImage

def getPieceImage(pieceName: String, isWhite: Boolean): Image = {
  val colorString = if (isWhite) "white" else "black"
  println(pieceName)
  val img = ImageIO.read(new File(s"src/main/scala/pieces/$colorString/$pieceName.png"))
  val resizedImg = img.getScaledInstance(64, 64, Image.SCALE_SMOOTH)
  val bImg = new BufferedImage(64, 64, BufferedImage.TYPE_INT_ARGB)
  val g2d = bImg.createGraphics()
  g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g2d.drawImage(resizedImg, 0, 0, null)
  g2d.dispose()
  Toolkit.getDefaultToolkit().createImage(bImg.getSource())
}
def drawChessBoardWithPieces(board: Array[Array[(Colors,Pieces)]]): Unit = {
  val darkSquare = new Color(209, 139, 71)
  val lightSquare = new Color(255, 206, 158)

  val letters = Array("a", "b", "c", "d", "e", "f", "g", "h")
  val numbers = Array("1", "2", "3", "4", "5", "6", "7", "8")
  val pieces = Map(
    'R' -> "rook",
    'N' -> "knight",
    'B' -> "bishop",
    'Q' -> "queen",
    'K' -> "king",
    'P' -> "pawn"
  )

  val boardGUI = new GridPanel(8, 8) {
    preferredSize = new Dimension(640, 640)
    override def paintComponent(g: Graphics2D) = {
      super.paintComponent(g)
      val pieceSize = 64
      for (i <- 0 until 8) {
        for (j <- 0 until 8) {
          val squareColor = if ((i + j) % 2 == 0) lightSquare else darkSquare
          g.setColor(squareColor)
          g.fillRect(j * pieceSize, i * pieceSize, pieceSize, pieceSize)
          for(i<-0 to 7){
            for(j<-0 to 7){
              if(board(i)(j)(0)!=Colors.Empty){
                g.drawImage(getPath(i, j, board), j * pieceSize, i * pieceSize, pieceSize, pieceSize, null)
              }
            }
          }
        }
      }
    }
  }


  val frame = new MainFrame {
    title = "Chess Board"
    contents = boardGUI
    pack()
    centerOnScreen()
    open()
    repaint()
  }
}
def getPath(i: Int, j: Int, board: Array[Array[(Colors,Pieces)]]):Image = board(i)(j) match{
  //  case (Colors.White, Pieces.Bishop) return
  case (Colors.White, Pieces.Rook) =>return getPieceImage("rook",true)
  case (Colors.White, Pieces.Knight) =>return getPieceImage("knight",true)
  case (Colors.White, Pieces.Bishop) =>return getPieceImage("bishop",true)
  case (Colors.White, Pieces.Queen) =>return getPieceImage("queen",true)
  case (Colors.White, Pieces.King) =>return getPieceImage("king",true)
  case (Colors.Black, Pieces.Rook) =>return getPieceImage("rook",false)
  case (Colors.Black, Pieces.Knight) =>return getPieceImage("knight",false)
  case (Colors.Black, Pieces.Bishop) =>return getPieceImage("bishop",false)
  case (Colors.Black, Pieces.Queen) =>return getPieceImage("queen",false)
  case (Colors.Black, Pieces.King) =>return getPieceImage("king",false)
  case _ => null
  //        case (1, _) => Console.RED + "♟" + Console.RESET
  //        case (6, _) => Console.BLUE + "♙" + Console.RESET
}
*/