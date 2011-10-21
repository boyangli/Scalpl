package analogy
import variable._
import plan._

class Matching (val prototype:Any, val gadget:Any)

case class Assignment (val argument:PopObject, val value:PopObject) extends Matching(argument, value)
{
  require(isValid())
  // in an assignment, the types of the argument and the value must be the same
  def isValid():Boolean = argument.pType == value.pType
}

case class Analogy (val prototype:PopObject, val gadget:PopObject) extends Matching (prototype, gadget)

case class ActionMatching (val prototype:Action, val gadget:Action) extends Matching (prototype, gadget) {
}

case class PropMatching (val protoProp:Proposition, val gadgetProp:Proposition) extends Matching (protoProp, gadgetProp) {
}

case class PrecondMatching (val protoAction:Action, val gadgetAction:Action, 
    val protoPred:Proposition, val gadgetPred:Proposition) extends Matching (protoPred, gadgetPred) {
}

case class EffectMatching (val protoAction:Action, val gadgetAction:Action, 
    val protoEffect:Proposition, val gadgetEffect:Proposition) extends Matching (protoEffect, gadgetEffect) {
}