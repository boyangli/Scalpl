package plan
import variable._

class Correspondence (val sourceAction:Action, val targetAction:Action, val sourceLink:Link, val targetLink:Link) {

}

class Matching 

class Assignment (val argument:PopObject, val value:PopObject) extends Matching
{
  require(isValid())
  // in an assignment, the types of the argument and the value must be the same
  def isValid():Boolean = argument.pType == value.pType
}

class Analogy (val source:PopObject, val target:PopObject) extends Matching