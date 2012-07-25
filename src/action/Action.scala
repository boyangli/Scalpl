package action
import variable._
import logging._
import planning._
import structures._
import parsing._

class Action(
  val id: Int,
  val name: String,
  val actor: Token,
  val parameters: List[Variable],
  val constraints: List[Proposition],
  val preconditions: List[Proposition],
  val effects: List[Proposition],
  val dummy: Boolean = false) extends Logging {

  testValid()

  // all variables referred in an action must appear in its parameter list
  def testValid() {
    // all distinct variables referred to by this action 
    val allvars = (preconditions ::: constraints ::: effects).flatMap(p => p.allVariables).distinct
    // those not contained in the parameters
    val newvars = allvars filterNot (x => parameters exists { y => x.name == y.name })
    //println("testing action " + name + " " + newvars)
    // they should not exist
    //println("params = " + parameters + " actor = " + actor + " new vars" + newvars)

    if (newvars.length != 0) {
      throw new PopParsingException("extra variables " + newvars.mkString(", ") + " in action " + name)

    } else if (actor != PopObject.unknown && !parameters.exists(p => p.name == actor.name)) {
      throw new PopParsingException("actor " + actor + " doesn't exist in parameter list in action " + name)
    }

    //(newvars.length == 0) && (actor == PopObject.unknown || parameters.exists(p => p.name == actor.name))
  }

  def instantiate(number: Int): Action =
    {
      new Action(
        number, name,
        actor match {
          case v: Variable => v instantiate number
          case o: Object => o
        },
        parameters.map(_ instantiate number),
        constraints.map(Action.instanProp(_, number)),
        preconditions.map(Action.instanProp(_, number)),
        effects.map(Action.instanProp(_, number)))
    }

  override def toString(): String =
    {
      "(" + name +
        (if (parameters.length > 0) " " + parameters.mkString(" ") else "") +
        ")"
    }

  def idText(): String = if (isGoal) "goal" else id.toString
  def isGoal(): Boolean = id == Constants.GOAL_ID

  def toShortString(): String =
    {
      "(" + name +
        (if (parameters.length > 0) " " + parameters.map(_.toShortString).mkString(" ") else "") +
        ")"
    }

  /**
   * constraints without neqs
   *
   */
  def pureConstraints() = constraints filterNot (_.verb == 'neq)

  /**
   * equality check based on name and id
   *
   */
  override def equals(that: Any) =
    that match {
      case a: Action => a.canEqual(this) && a.name == this.name && a.id == this.id
      case _ => false
    }

  def canEqual(that: Any) = that.isInstanceOf[Action]
}

object Action {

  def initState(props: List[Proposition]) = new Action(Constants.INIT_ID, "init-state", PopObject.unknown,
    List[Variable](), List[Proposition](), List[Proposition](), props, true)

  def apply(id: Int, name: String, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition], dummy: Boolean): Action =
    new Action(id, name, PopObject.unknown, parameters, constraints, preconditions, effects, dummy)

  def apply(id: Int, name: String, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition]): Action =
    new Action(id, name, PopObject.unknown, parameters, constraints, preconditions, effects)

  def apply(name: String, actor: Token, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition]): Action =
    new Action(-1, name, actor, parameters, constraints, preconditions, effects)

  def apply(name: String, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition]): Action =
    new Action(-1, name, PopObject.unknown, parameters, constraints, preconditions, effects)

  private def instanProp(prop: Proposition, number: Int): Proposition =
    {
      val verb = prop.verb
      val termlist: List[TopTerm] = prop.termlist.map(_ match {
        case v: Variable => v.instantiate(number)
        case s: PopObject => s
        case p: Proposition => instanProp(p, number)
        case _ => throw new Exception("weird content in term list")
      })
      Proposition(verb, termlist)
    }
}

class DecompAction(
  id: Int,
  name: String,
  actor: Token,
  parameters: List[Variable],
  constraints: List[Proposition],
  preconditions: List[Proposition],
  effects: List[Proposition],
  val composite: Boolean) extends Action(id, name, actor, parameters, constraints, preconditions, effects) {

  override def toString(): String =
    "(" +
      { if (composite) "[C]" else "" } +
      name +
      {
        if (parameters.length > 0) " " +
          parameters.mkString(" ")
        else
          ""
      } + ")"
}

object DecompAction {

  def apply(id: Int, name: String, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition], composite: Boolean): DecompAction =
    new DecompAction(id, name, PopObject.unknown, parameters, constraints, preconditions, effects, composite)

  def apply(name: String, actor: Token, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition], composite: Boolean): DecompAction =
    new DecompAction(-1, name, actor, parameters, constraints, preconditions, effects, composite)

  def apply(name: String, parameters: List[Variable], constraints: List[Proposition],
    preconditions: List[Proposition], effects: List[Proposition], composite: Boolean): DecompAction =
    new DecompAction(-1, name, PopObject.unknown, parameters, constraints, preconditions, effects, composite)
}