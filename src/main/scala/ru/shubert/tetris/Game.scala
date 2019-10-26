package ru.shubert.tetris

import java.awt.{Dimension, Graphics}
import java.awt.image.BufferedImage
import scala.swing.BorderPanel
import scala.swing.event.{Key, KeyPressed, FocusLost}

/**
 * BUGS:
 * 1. Rotations seems to be un-centered. => move coord center to the figure center.
 * 2. it's possible to rotate figures through the function
 * 3. glass may be a one dimension array.
 */
object Game extends BorderPanel {
  val LeftBorder = 244
  val RightBorder = 403
  val BottomBorder = 438
  val TopBorder = 49

  val NextFigureX = 549
  val NextFigureY = 121

  var paused = false
  var inGame = false
  val gameSpeed = 700
  val delayPerLevel = 40

  val rows = 24
  val cols = 10
  // X and Y are inverted, -1 for free cell
  var glass: Array[Array[Int]] = Array.fill(rows, cols)(-1)
  val timer = ScalaSwingTimer(gameSpeed)

  preferredSize = new Dimension(640, 480)
  focusable = true
  listenTo(keys, timer, this)
  reactions += {
    case _: FocusLost =>
      paused = false
      switchPaused()
    case _: ScalaSwingTimerEvent => tick()
    case KeyPressed(_, Key.Up, _, _) => ifRunning(Brix.rotate)
    case KeyPressed(_, Key.Left, _, _) => ifRunning(Brix.moveLeft)
    case KeyPressed(_, Key.Right, _, _) => ifRunning(Brix.moveRight)
    case KeyPressed(_, Key.Down, _, _) => ifRunning(Brix.moveDown)
    case KeyPressed(_, Key.Escape, _, _) => switchPaused()
    case KeyPressed(_, Key.S, _, _) => startNewGame()
    case KeyPressed(_, Key.M, _, _) => Sound.switchSound()
  }

  def ifRunning(func: () => Unit): Unit = if (!paused && inGame) {
    func()
    repaint()
  }

  def switchPaused() {
    paused = !paused
    if (paused) timer.stop() else timer.start()
    repaint()
  }

  /**
   * Renders flashing string
   * @param g graphic context
   */
  def renderPaused(g: Graphics) {
    val x = (peer.getWidth - Art.pause.getWidth(null)) / 2 + 4
    g.drawImage(Art.pause, x, (peer.getHeight - Art.pause.getHeight(null)) / 2, null)
  }

  def renderStartGame(g: Graphics) {
    g.drawImage(Art.press_s, (640 - Art.press_s.getWidth(null)) / 2 + 5, 250, null)
  }

  /**
   * Figure rendering
   * @param g graphic context
   * @param baseX  X offset
   * @param baseY  Y offset
   */
  def renderFigure(g: Graphics, baseX: Int, baseY: Int) {
    Brix.getCurrent foreach {
      case (x: Int, y: Int) =>
        val realX = (baseX * Brix.size) + Game.LeftBorder
        val realY = (baseY * Brix.size) + Game.TopBorder
        val offsetX: Int = x * Brix.size
        val offsetY: Int = y * Brix.size
        g.drawImage(Art.colors(Brix.curFigure.c), realX + offsetX, realY + offsetY, null)
    }
  }

  def renderNextFigure(g: Graphics) {
    Brix.getNext foreach {
      case (x: Int, y: Int) =>
        val realX = NextFigureX + Brix.size * x
        val realY = NextFigureY + Brix.size * y
        g.drawImage(Art.colors(Brix.nextFigure.c), realX, realY, null)
    }
  }

  private def renderDigits(g: Graphics, value: Int, x: Int, y: Int) {
    val count = Math.log10(value).shortValue // log10(0) == NegInfinity, short truncates it to 0

    for {
      pow <- 0 to count
      digit = (value / Math.pow(10, pow)) % 10
    } g.drawImage(Art.digits(digit.toInt), x - 12 * pow, y, null)
  }

  def renderScore(g: Graphics) {
    renderDigits(g, Stat.points, 593, 341)
  }

  def renderLevel(g: Graphics) {
    renderDigits(g, Stat.level, 592, 243)
  }

  def renderGlass(g: Graphics) {
    for {
      row: Int <- 0 until rows
      col: Int <- 0 until cols
      if glass(row)(col) >= 0
    } g.drawImage(Art.colors(glass(row)(col)),
      Game.LeftBorder + col * Brix.size,
      Game.TopBorder + row * Brix.size,
      null)
  }

  def renderGameOver(g: Graphics) {
    val x = (640 - Art.gameover.getWidth(null)) / 2 + 5
    val y = 150
    g.drawImage(Art.gameover, x, y, null)
  }

  def truncateLines(): Unit = {
    def truncate(minY: Int, maxY: Int): Int = {
      var rows = 0
      for {
        x <- minY to maxY
        if glass(x) forall (_ > -1)
      } {
        for (pos <- x to 1 by -1)
          glass(pos - 1) copyToArray glass(pos)
        rows += 1
      }
      rows
    }

    val (minY, maxY) = ((0, 0) /: Brix.getCurrent) {
      case ((nY: Int, mY: Int), (_, y: Int)) => (nY min y, mY max y)
    }

    val r = truncate(Brix.currentY + minY, Brix.currentY + maxY)
    if (r > 0) {
      Stat updateScore r
      Sound.playTrunc(r)
      val newDelay = (gameSpeed - Stat.level * delayPerLevel) max 1
      timer.setDelay(newDelay)
    }
  }

  private def resetGlass(toRow: Int = rows - 1) {
    for {
      r <- 0 to toRow
      c <- 0 until cols
    } glass(r)(c) = -1
  }

  def bindFigure(): Unit = {
    Brix.getCurrent foreach {
      case (x: Int, y: Int) =>
        glass(Brix.currentY + y)(Brix.currentX + x) = Brix.curFigure.c
    }
    Sound.playBind()
  }

  def isGameOver: Boolean = glass(1) exists (_ > -1)

  def startNewGame() {
    if (!inGame) {
      if (isGameOver) Brix.reset()
      Stat.reset()
      resetGlass()

      inGame = true
      paused = false
      timer.setDelay(gameSpeed)
      timer.start()
      repaint()
    }
  }

  def stopGame() {
    timer.stop()
    inGame = false
    Sound.playGameOver()
  }

  def tick(): Unit = {
    if (!(paused || isGameOver)) {
      if (Brix.mayDown())
        Brix.moveDown()
      else {
        bindFigure()
        truncateLines()
        if (isGameOver) stopGame() else Brix.newFigure()
      }
    }
    repaint()
  }


  override def paint(g: _root_.scala.swing.Graphics2D): Unit = {
    // buffering image
    val image = new BufferedImage(peer.getWidth, peer.getHeight, BufferedImage.TYPE_INT_RGB)
    val imageG = image.getGraphics
    imageG.drawImage(Art.bg2, 0, 0, null)

    renderGlass(imageG)
    renderFigure(imageG, Brix.currentX, Brix.currentY)
    renderScore(imageG)
    renderNextFigure(imageG)
    renderLevel(imageG)

    if (inGame && paused) {
      renderPaused(imageG)
    } else if (isGameOver) {
      renderGameOver(imageG)
      renderStartGame(imageG)
    } else if (!inGame) {
      renderStartGame(imageG)
    }

    imageG.dispose()
    g.drawImage(image, 0, 0, null)
  }
}