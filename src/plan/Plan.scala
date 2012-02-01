package plan
import variable._

case class Plan(
    val id: Int,
    val steps: List[Action],
    val links: List[Link],
    val ordering: Ordering,
    val binding: Binding,
    val flaws: List[Flaw],
    val reason: String,
    val history: List[Record],
    val parent: Plan,
    var children: List[Plan],
    val stepCount: Int = 0) {

  //var stepCount = 0;
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
      for (i <- order if i != 0 && i != Global.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => desc += "[" + i + "] " + binding.substVars(x) + "\n"
          case _ => ""
        }
      }
      desc
    }

  def id2step(id: Int): Option[Action] = steps.find { _.id == id }

  def initialState() = steps.find(_.id == 0).get.effects // initial state
}

object Plan {
  //  def apply(steps: List[Action], links: List[Link], ordering: Ordering):Plan =
  //  {
  //    // TODO: set the id properly
  //    new Plan(-1, steps, links, ordering, List[Flaw](), "", null)
  //  }

  def getEmpty(): Plan =
    {
      val id = Global.obtainID()
      new Plan(id, List[Action](), List[Link](), new Ordering(), new Binding(), List[Flaw](), "", List[Record](), null, null)
    }
}

