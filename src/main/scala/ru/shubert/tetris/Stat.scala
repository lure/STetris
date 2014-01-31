package ru.shubert.tetris


object Stat {
  var points: Int = 0
  var figures: Int = 0
  var lines: Int = 0
  val gameTurn = 15


  def reset() {
    points = 0
    figures = 0
    lines = 0
  }

  def level: Int = lines / gameTurn + 1

  def updateScore(lCount: Int) {
    if (lCount > 0) {
      lines += lCount

      if (lCount < 5)
        points += 10 + (lCount - 1) * 15
      else {
        throw new Error("more than 4 lines shortened! Lines == " + lCount)
      }
    }
  }
}