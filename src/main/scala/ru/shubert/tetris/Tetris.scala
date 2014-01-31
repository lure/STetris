package ru.shubert.tetris

import scala.swing._


object Tetris extends SimpleSwingApplication {

  def top = new MainFrame {
    contents = Game
    resizable = false
    centerOnScreen()
  }
}