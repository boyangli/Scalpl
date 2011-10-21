package plan
import variable._
import plan._

class Matching 

case class Assignment (val argument:PopObject, val value:PopObject) extends Matching
{
  require(isValid())
  // in an assignment, the types of the argument and the value must be the same
  def isValid():Boolean = argument.pType == value.pType
}

case class Analogy (val source:PopObject, val target:PopObject) extends Matching

case class ActionMatching (val source:Action, val target:Action) extends Matching {
}

case class PropMatching (val sourceProp:Proposition, val targetProp:Proposition) extends Matching {
}

case class PrecondMatching (val sourceAction:Action, val targetAction:Action, 
    val sourcePred:Proposition, val targetPred:Proposition) extends Matching {
}

case class EffectMatching (val sourceAction:Action, val targetAction:Action, 
    val sourceEffect:Proposition, val targetEffect:Proposition) extends Matching {
}