package ru.shubert.tetris

import javax.sound.sampled.{Line, Clip, AudioSystem}
import java.io._

// http://docs.oracle.com/javase/tutorial/sound/accessing.html
// http://www.freesound.org/browse/packs/?order=-last_update&page=3#pack
// http://www.onjava.com/pub/a/onjava/2004/08/11/javasound-mp3.html?page=2
object Sound {
  private var enabled = true
  private val explode = load("explode.wav")
  private val bind = load("bind.wav")
  private val shorten = load("shorten.wav")
  private val gameOver = load("gameover.wav")

  private def load(name: String): Clip = {
    val as = AudioSystem getAudioInputStream Sound.getClass.getResource(name)
    val line = AudioSystem getLine new Line.Info(classOf[Clip])
    val clip = line.asInstanceOf[Clip]

    val format = as.getFormat
    val baos = new ByteArrayOutputStream()
    val arr = Array.ofDim[Byte](1024)

    Iterator.continually({
      as.read(arr, 0, 1024)
    }).takeWhile(_ != -1).foreach((x: Int) => baos.write(arr, 0, x))

    clip.open(format, baos.toByteArray, 0, baos.size())
    as.close()
    baos.close()
    clip
  }


  private def play(clip: Clip) {
    if (enabled) {
      clip stop()
      clip setFramePosition 0
      clip start()
    }
  }

  def playBind() = play(bind)

  def playGameOver() = play(gameOver)

  def playTrunc(level: Int) = play(soundMap(level))

  private val soundMap = Map[Int, Clip](
    1 -> shorten,
    2 -> shorten,
    3 -> shorten,
    4 -> explode
  ).withDefaultValue(shorten)

  def switchSound(): Boolean = {
    enabled = !enabled
    enabled
  }

  override def finalize(): Unit = {
    super.finalize()
    bind close()
    explode close()
    shorten close()
  }
}

