package plan

import variable._

class Flaw(protected var level:Int) {
  
	def priority() = level
}

class Threat (val id:Int, val effect:Proposition, val threatened:Link) extends Flaw(10)
{
  override def toString():String = "<Threat: step" + id + " " + effect + "-X-" + threatened + ">"
}

class OpenCond(val id:Int, val condition:Proposition) extends Flaw(20)
{
  override def toString():String = "<Open " + condition + " from step " + 
  (if (id == Global.GOAL_ID) "goal" else id) + ">"
}