package plan
import variable._

class Action(
    val id: Int,
    val name: String,
    val actor: Token,
    val parameters: List[Variable],
    val constraints: List[Proposition],
    val preconditions: List[Proposition],
    val effects: List[Proposition]) {

  require(isValid())

  def isValid(): Boolean =
    {
      // all distinct variables referred to by this action 
      val allvars = (preconditions ::: constraints ::: effects).flatMap(p => p.allVariables).distinct
      // those not contained in the parameters
      val newvars = allvars filterNot (x => parameters exists { y => x.name == y.name })
      //println("testing action " + name + " " + newvars)
      // they should not exist
      //println("actor = " + actor)
      (newvars.length == 0) && (actor == PopObject.unknown || parameters.exists(p => p.name == actor.name))
    }

  def instantiate(number: Int): Action =
    {
      new Action(
        number, name, 
        actor match{
          case v:Variable => v instantiate number
          case o:Object => o
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
  def isGoal(): Boolean = id == Global.GOAL_ID

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
}

object Action {

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