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

  /** this overrides the old copy method in Plan, so the dlink field is copied by default
   * 
   */
  override def copy(
    id: Int = this.id,
    steps: List[Action] = this.steps,
    links: List[Link] = this.links,
    ordering: Ordering = this.ordering,
    binding: Binding = this.binding,
    flaws: List[Flaw] = this.flaws,
    reason: String = this.reason,
    history: List[Record] = this.history,
    parent: Plan = this.parent,
    children: List[Plan] = this.children,
    stepCount: Int = this.stepCount): DecompPlan =
    {
      new DecompPlan(id, steps, links, this.dlinks, ordering, binding, flaws, reason, history, parent, children, stepCount)
    }

  /** Interestingly, Scala would not allow me to overload the copy method. 
   * This is why there is a decompCopy, which supports copying every field in DecompPlan
   */
  def decompCopy(
    id: Int = this.id,
    steps: List[Action] = this.steps,
    links: List[Link] = this.links,
    dlinks: List[DecompLink] = this.dlinks,
    ordering: Ordering = this.ordering,
    binding: Binding = this.binding,
    flaws: List[Flaw] = this.flaws,
    reason: String = this.reason,
    history: List[Record] = this.history,
    parent: Plan = this.parent,
    children: List[Plan] = this.children,
    stepCount: Int = this.stepCount): DecompPlan =
    {
      new DecompPlan(id, steps, links, dlinks, ordering, binding, flaws, reason, history, parent, children, stepCount)
    }
  
  def ultimateParent(stepId:Int):Int = 
  {
    var kid = stepId
    var parent = dlinks.find(dl => dl.children.contains(kid))
    while(parent.isDefined)
    {
      kid = parent.get.parent
      parent = dlinks.find(dl => dl.children.contains(kid))
    }
    kid
  }

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

