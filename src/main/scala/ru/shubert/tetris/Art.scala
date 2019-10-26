package ru.shubert.tetris

import javax.imageio.ImageIO
import java.awt.Image

object Art {
  val brix: Image = load("brix.png").getScaledInstance(16, 16, Image.SCALE_DEFAULT)
  val bg2: Image = load("tetris.png")
  val gameover: Image = load("gameover.png")
  val pause: Image = load("pause.png")
  val press_s: Image = load("press_s.png")
  val digits: Array[Image] = loadDigits()
  val colors: Array[Image] = loadColors()

  def load(path: String): Image = {
    ImageIO.read(Art.getClass.getResource(path))
  }

  def loadDigits(): Array[Image] = {
    val array = Array.ofDim[Image](10)
    for (x <- 0 to 9) {
      array(x) = load(x + ".png")
    }
    array
  }

  def loadColors(): Array[Image] = {
    val colors = Array("blue.png", "cyan.png", "green.png", "lilac.png", "orange.png", "red.png", "yellow.png")
    val array = Array.ofDim[Image](colors.length)
    for (x <- colors.indices) {
      array(x) = load(colors(x))
    }
    array
  }
}
