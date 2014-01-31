package ru.shubert.tetris

import scala.Array
import scala.util.{Try, Random}

/**
 * Geometric rotation makes somefigures (square, Z, S) 'jump'
 * x` = x cos t - y sin t
 * y` = x sin t + y cos t
 * cos 90 = 0, sin 90 = 1  cos 270 = 0, sin 270 = -1
 * CAVEAT: rotate is uncentered for Z,S, cube and so on
 */
object Brix {
  // width & height in pixels
  val size = 16
  val startX = 4
  val startY = 1

  sealed trait RotateTo

  case object RotateLeft extends RotateTo

  case object RotateRight extends RotateTo

  type Figure = Array[(Int, Int)]
  type Rotations = Array[Figure]

  sealed case class Piece(f: Int, r: Int, c: Int)

  val figs: Array[Rotations] = Array(
    Array(Array((0, 0), (0, 1), (1, 1), (1, 0))), // cube
    Array(
      Array((-1, 0), (0, 0), (1, 0), (2, 0)), // horizontal line
      Array((0, -2), (0, -1), (0, 0), (0, 1)) // vertical line
    ),
    Array(
      Array((-1, 0), (0, 0), (0, 1), (1, 1)), // Z horiz
      Array((1, -1), (1, 0), (0, 0), (0, 1)) // Z vert
    ),
    Array(
      Array((-1, 1), (0, 1), (0, 0), (1, 0)), // S horiz
      Array((-1, -1), (-1, 0), (0, 0), (0, 1)) // S vert
    ),
    Array(
      Array((-1, 0), (0, 0), (1, 0), (1, 1)), //L
      Array((0, -1), (0, 0), (0, 1), (-1, 1)), //L
      Array((-1, -1), (-1, 0), (0, 0), (1, 0)), //L
      Array((1, -1), (0, -1), (0, 0), (0, 1)) //L
    ),
    Array(
      Array((-1, 0), (0, 0), (1, 0), (1, -1)), //inverted L pin up
      Array((-1, -1), (0, -1), (0, 0), (0, 1)), //inverted L pin left
      Array((-1, 1), (-1, 0), (0, 0), (1, 0)), //inverted L pin down
      Array((0, -1), (0, 0), (0, 1), (1, 1)) //inverted L pin right
    ),
    Array(
      Array((-1, 0), (0, 0), (1, 0), (0, 1)), // T pin down
      Array((0, -1), (0, 0), (0, 1), (-1, 0)), // T pin left
      Array((-1, 0), (0, 0), (1, 0), (0, -1)), // T pin up
      Array((0, -1), (0, 0), (0, 1), (1, 0)) // T pin right
    )
  )

  val random = new Random(System.nanoTime())
  var nextFigure: Piece = Piece(random.nextInt(figs.size), 0, random.nextInt(Art.colors.length))
  var curFigure: Piece = Brix.newFigure()
  var currentX = startX
  var currentY = startY

  def rotate() {
    rotateFigure(RotateRight)
  }

  /**
   * rotates figure clock or anti clockwise depending on direction
   * NOTE: rotating long figures makes them 'jump' up. Check L for example
   * @param dir rotation direction
   * @return next/previous rotation index depending on direction
   */
  private def rotateFigure(dir: RotateTo = RotateRight) {
    val rot = curFigure.r
    val rotCount = figs(curFigure.f).size

    val newRotation = dir match {
      case RotateLeft => if (rot == 0) rotCount - 1 else rot - 1
      case RotateRight => (rot + 1) % rotCount
      case _ => throw new Error("Unknown direction " + dir)
    }

    if (maybePlacedAt(figs(curFigure.f)(newRotation), currentY, currentX))
      curFigure = Piece(curFigure.f, newRotation, curFigure.c)
    else if (maybePlacedAt(figs(curFigure.f)(newRotation), currentY, currentX + 1)) {
      curFigure = Piece(curFigure.f, newRotation, curFigure.c)
      moveRight()
    } else if (maybePlacedAt(figs(curFigure.f)(newRotation), currentY, currentX - 1)) {
      curFigure = Piece(curFigure.f, newRotation, curFigure.c)
      moveLeft()
    }
  }

  private def next(): Piece = {
    Piece(random.nextInt(figs.size), 0, random.nextInt(Art.colors.length))
  }

  /**
   * returns indexes for a new random figure and random rotation for it
   * @return pair (figureIndex, rotationIndex)
   */
  def newFigure(): Piece = {
    curFigure = nextFigure
    nextFigure = next()

    currentX = startX
    currentY = startY
    curFigure
  }

  def moveLeft() {
    if (mayLeft()) currentX -= 1
  }

  def moveRight() {
    if (mayRight()) currentX += 1
  }

  def moveDown() {
    if (mayDown()) currentY += 1
  }

  def getCurrent: Figure = figs(curFigure.f)(curFigure.r)

  def getNext: Figure = figs(nextFigure.f)(nextFigure.r)

  def mayDown(): Boolean = {
    val picks = Brix.getCurrent groupBy (_._1)

    picks forall {
      case (xValue, array) =>
        val yValue = array.maxBy(_._2)._2
        Try[Boolean] {
          Game.glass(currentY + yValue + 1)(currentX + xValue) == -1
        }.getOrElse(false)
    }
  }

  def mayLeft(): Boolean = {
    val picks = Brix.getCurrent groupBy (_._2)

    picks forall {
      case (yValue, array) =>
        val xValue = array.minBy(_._1)._1
        Try[Boolean] {
          Game.glass(currentY + yValue)(currentX + xValue - 1) == -1
        }.getOrElse(false)
    }
  }

  def mayRight(): Boolean = {
    val picks = Brix.getCurrent groupBy (_._2)

    picks forall {
      case (yValue, array) =>
        val xValue = array.maxBy(_._1)._1
        Try[Boolean] {
          Game.glass(currentY + yValue)(currentX + xValue + 1) == -1
        }.getOrElse(false)
    }
  }

  def maybePlacedAt(figure: Figure, row: Int, col: Int): Boolean = {
    figure forall {
      case (x: Int, y: Int) => Try[Boolean] {
        Game.glass(row + y)(col + x) == -1
      }.getOrElse(false)
    }
  }

  def reset() {
    newFigure()
    newFigure()
  }
}
