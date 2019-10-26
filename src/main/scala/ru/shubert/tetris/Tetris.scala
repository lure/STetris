package ru.shubert.tetris

import scala.swing._


object Tetris extends SimpleSwingApplication {

  def top: MainFrame = new MainFrame {
    contents = Game
    resizable = false
    centerOnScreen()
  }
}