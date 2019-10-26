package ru.shubert.tetris

import javax.swing.Timer
import scala.swing.{Reactor, Publisher}
import java.awt.event.{ActionEvent, ActionListener}
import scala.swing.event.Event

sealed case class ScalaSwingTimerEvent(source: AnyRef) extends Event

/**
 * Dirty scala wrapper.
 * @param delay initial delay. May be changed during execution/
 */
class ScalaSwingTimer private(delay: Int) extends Timer(delay, null) with Publisher {
  _this =>

  addActionListener(new ActionListener {
    def actionPerformed(e: ActionEvent): Unit = publish(ScalaSwingTimerEvent(_this))
  })
}

object ScalaSwingTimer extends Reactor {
  def apply(delay: Int) = new ScalaSwingTimer(delay)
}