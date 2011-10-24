package analogy

import variable._
import logging._
import plan._

object AnalogyEngine extends Logging {

  def goodThreshold = 0.4
  def evalAnalogy(p1: Proposition, p2: Proposition): Double =
    {
      debug { "analogically comparing " + p1 + " and " + p2 }
      0.5
    }
  def evalAnalogy(a1: Action, a2: Action): Double =
    {
      debug { "analogically comparing " + a1 + ", " + a1.name + " and " + a2 + ", " + a2.name}
      if (a1.name == "infected-by" && a2.name == "hear") 0.7
      else 0.3
    }
}