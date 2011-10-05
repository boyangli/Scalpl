package plan
import variable._


case class Frame(
  val name: String,
  val gadget: PopObject,
  override val steps: List[Action],
  override val links: List[Link],
  override val ordering: Ordering,
  override val binding: Binding) extends PlanLike(steps, links, ordering, binding) {
  
}