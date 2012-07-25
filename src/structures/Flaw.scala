package structures

import variable._
import planning._

class Flaw(protected var level: Int) {

  /** higher priorities are smaller in number
   * 
   */
  def priority() = level
}

class Threat(val id: Int, val effect: Proposition, val threatened: Link) extends Flaw(10) {
  override def toString(): String = "<Threat: step" + id + " " + effect + "-X-" + threatened + ">"
}

class OpenCond(val id: Int, val condition: Proposition) extends Flaw(20) {
  override def toString(): String = "<Open " + condition + " from step " +
    (if (id == Constants.GOAL_ID) "goal" else id) + ">"
}

class UnDecomposed(val id: Int) extends Flaw(15) {
  override def toString(): String = "<UnDecomposed: step" + id + ">"
}