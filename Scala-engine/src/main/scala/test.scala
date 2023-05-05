import swing._
import scala.swing.event._

object Board extends MainFrame {
  title = "8x8 Board"
  preferredSize = new Dimension(400, 400)

  val boardSize = 8
  val tileSize = 50

  val boardPanel = new Panel {
    preferredSize = new Dimension(boardSize * tileSize, boardSize * tileSize)

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      for {
        x <- 0 until boardSize
        y <- 0 until boardSize
      } {
        if ((x + y) % 2 == 0) g.setColor(Color.white)
        else g.setColor(Color.black)
        g.fillRect(x * tileSize, y * tileSize, tileSize, tileSize)
      }
    }
  }

  contents = boardPanel

  centerOnScreen()
  open()
}
