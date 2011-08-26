package plan

import variable._

class Link (
val id1:Int,
val id2:Int,
val condition:Proposition
) {

  override def toString():String = "(" + id1 + " -> " + (if(id2 == Global.GOAL_ID) "goal" else id2) + ": " + condition + ")"
}