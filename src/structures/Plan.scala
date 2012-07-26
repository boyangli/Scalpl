package structures
import variable._
import analogy._
import action._
import planning._
import parsing.Parsible

class PlanLike(
  val steps: List[Action],
  val links: List[Link],
  val ordering: Ordering,
  val binding: Binding) {
}

case class ProtoFrame(
  val name: String,
  override val steps: List[Action],
  override val links: List[Link],
  override val ordering: Ordering,
  override val binding: Binding) extends PlanLike(steps, links, ordering, binding) {

  override def toString(): String = "<Frame of " + name + " #steps=" + steps.length + ">"

  def planString(): String =
    {
      // a plan without steps
      if (steps.length <= 2) return "Frame of " + name + " with 0 steps"

      var desc = "<Frame of " + name + ">\n"

      val order = ordering.topsort()
      for (i <- order if i != 0 && i != Constants.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => desc += "[" + i + "] " + binding.substVarsString(x) + "\n"
          case _ => ""
        }
      }
      desc
    }
}

case class Plan(
  val id: Int,
  override val steps: List[Action],
  override val links: List[Link],
  override val ordering: Ordering,
  override val binding: Binding,
  val flaws: List[Flaw],
  val reason: String,
  val history: List[Record],
  val parent: Plan,
  var children: List[Plan],
  val stepCount: Int = 0) extends PlanLike(steps, links, ordering, binding) with Parsible {

  override def toString(): String = "<Plan[" + id + "] #steps=" + stepCount + ", #flaws=" + flaws.length + ">"

  def detailString(): String =
    {
      var desc = planString() + "\n"
      desc += flaws.mkString("flaws: \n", "\n", "\n")
      desc += links.mkString("links: \n", "\n", "\n")
      desc += "orderings: \n" + ordering.toString() + "\n"
      desc += "reason: " + reason + "\n"
      desc += "history: " + history.mkString(", ") + "\n"
      desc
    }

  def planString(): String =
    {
      // a plan without steps
      if (steps.length <= 2) return "Plan[" + id + "] with 0 steps"

      var desc = ""
      //println(ordering.allIDs)
      val order = ordering.topsort()
      //print("order "+ order)
      for (i <- order if i != Constants.INIT_ID && i != Constants.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => desc += "[" + i + "] " + binding.substVarsString(x) + "\n"
          case _ => ""
        }
      }
      desc
    }

  /*
  def getEmpty(g:GlobalInfo): Plan =
    {
      val id = g.newPlanID()
      new Plan(id, List[Action](), List[Link](), new Ordering(), new Binding(), List[Flaw](), "",
        List[Record](), null, null)
    }
    */

  override def toParseString(): String =
    {
      var answer = "(objects " + collectObjects.mkString(" ") + ")\n" // objects
      answer += "(initial-state " + initialState.map(_.toShortString).mkString(" ") + ")\n" // initial state

      val order = ordering.topsort()
      var stepString = ""

      for (i <- order if i != Constants.INIT_ID && i != Constants.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => stepString += "[" + i + "] " + binding.substVarsShortString(x) + "\n"
          case _ => ""
        }
      }

      answer += "(goal-state " + goalState.map(_.toShortString).mkString(" ") + ")\n"
      answer += "(steps \n" + stepString + ")\n"
      answer += "(links " + links.map(_.toFileString).mkString("\n") + ")\n"
      answer += "(orderings " + ordering.toFileString() + ")\n"

      answer
    }

  private def collectObjects(): Set[PopObject] =
    {
      var collection = Set[PopObject]()
      // collect objects from initial / goal states
      (initialState ::: goalState) foreach { prop =>
        collection ++= prop.allObjects()
      }

      // collect objects from all steps
      steps foreach {
        _.parameters foreach {
          _ match {
            case o: PopObject => collection += o
            case _ =>
          }
        }
      }

      collection
    }

  def id2step(id: Int): Option[Action] = steps.find { _.id == id }

  def initialState() = steps.find(_.id == 0).get.effects // initial state
  def goalState() = steps.find(_.id == Constants.GOAL_ID).get.preconditions // goal state
}

object Plan {

  def apply(id: Int, steps: List[Action], links: List[Link], ordering: Ordering, binding: Binding, flaws: List[Flaw], reason: String,
    parent: Plan, children: List[Plan]) =
    new Plan(id, steps, links, ordering, binding, flaws, reason, List[Record](), parent, children, 
        steps filterNot{_.dummy} size)

  def getEmpty(global:GlobalInfo): Plan =
    {
      val id = global.newPlanID()
      new Plan(id, List[Action](), List[Link](), new Ordering(), new Binding(), List[Flaw](), "", List[Record](), null, null)
    }
}
