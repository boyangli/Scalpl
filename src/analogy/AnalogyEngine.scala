package analogy

import variable._
import plan._

object AnalogyEngine {

  def goodThreshold = 0.6
  def evalAnalogy(p1: Proposition, p2: Proposition): Double = 0.5
  def evalAnalogy(a1: Action, a2:Action):Double = 0.5
}