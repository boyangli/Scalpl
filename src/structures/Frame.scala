package structures
import variable._
import action._
import jimpl._

case class Frame(
  val name: String,
  val gadget: PopObject,
  override val steps: List[Action],
  override val links: List[Link],
  override val ordering: JOrdering,
  override val binding: Binding) extends PlanLike(steps, links, ordering, binding) {
  
}