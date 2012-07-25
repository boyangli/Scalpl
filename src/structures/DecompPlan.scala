package structures
import action._
import planning._
import variable._

class DecompPlan(
  id: Int,
  steps: List[Action],
  links: List[Link],
  val dlinks: List[DecompLink],
  ordering: Ordering,
  binding: Binding,
  flaws: List[Flaw],
  reason: String,
  history: List[Record],
  parent: Plan,
  children: List[Plan],
  stepCount: Int = 0) extends Plan(id, steps, links, ordering, binding, flaws, reason, history, parent, children, stepCount) {

}

object DecompPlan {
  def apply(id: Int, steps: List[Action], links: List[Link], dlinks: List[DecompLink], ordering: Ordering,
    binding: Binding, flaws: List[Flaw], reason: String,
    parent: Plan, children: List[Plan]) =
    new DecompPlan(id, steps, links, dlinks, ordering, binding, flaws, reason, List[Record](), parent, children)

  def getEmpty(global: GlobalInfo): Plan =
    {
      val id = global.newPlanID()
      new DecompPlan(id, List[Action](), List[Link](), List[DecompLink](), new Ordering(), new Binding(), List[Flaw](), "", List[Record](), null, null)
    }
}

