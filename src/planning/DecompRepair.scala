package planning
import variable._
import action._
import logging._
import structures._

/**
 * repair algorithms for decompositional partial order planner (Young & Pollack 1996)
 * TODO: Test these methods. Add reuse.
 */
object DecompRepair extends Logging {

  def refine(g: DecompGlobal)(p: Plan): List[Plan] =
    {
      if (!p.isInstanceOf[DecompPlan])
        throw new DecompException("The plan is not an instance of DecompPlan")
      if (!g.isInstanceOf[DecompGlobal])
        throw new DecompException("The global object is not an instance of DecompGlobal")

      val kids =
        SimpleRepair.selectFlaw(p) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            repairOpen(p, open, g)
          case threat: Threat =>
            trace("repairing: " + threat)
            // threats are repaired in the same old way
            SimpleRepair.repairThreat(p, threat, g)
          case undecomposed: UnDecomposed =>
            trace("repairing: " + undecomposed)
            repairUnDecomposed(p.asInstanceOf[DecompPlan], undecomposed, g.asInstanceOf[DecompGlobal])
          case _ => throw new Exception("A flaw with no known repairs! " + p.flaws)
        }
      p.children = kids

      trace {
        "new plans: " +
          kids.map {
            plan =>
              plan.toString + "\n" +
                plan.planString()
          } mkString
      }

      kids
    }

  def repairUnDecomposed(p: DecompPlan, und: UnDecomposed, g: DecompGlobal): List[Plan] =
    {
      // find a decomposition recipe
      val parent = p.id2step(und.id).get
      val reasonString = "decomposing step " + und.id
      val recipes = g.recipes.filter(r => r.name == parent.name)
      // decompose it
      recipes map {
        recipe =>

          var highStep = p.stepCount
          val newSteps = for(s <- recipe.steps) yield {
            highStep += 1
            s.doubleInstantiate(highStep, parent.id)
          }

          val newLinks = recipe.links map {
            link =>
              val id1 = newSteps(link.id1).id
              val id2 = newSteps(link.id2).id
              val cond = link.precondition instantiate parent.id
              new Link(id1, id2, cond, cond)
          }

          val newOrderings = p.ordering.list ++ (recipe.ordering map {
            order: (Int, Int) =>
              val id1 = newSteps(order._1).id
              val id2 = newSteps(order._2).id
              (id1, id2)
          })

          val undecomp = newSteps filter { _.composite } map { s => new UnDecomposed(s.id) }
          val open = newSteps flatMap { step =>
            val existing = newLinks filter { l => l.id2 == step.id } map { _.precondition }
            val remainder = step.preconditions filterNot { existing contains }
            remainder map { cond => new OpenCond(step.id, cond) }
          }

          val newDlink = new DecompLink(parent.id, newSteps map { _.id })

          val kid = p.decompCopy(
            id = g.newPlanID(),
            steps = newSteps ::: p.steps,
            links = newLinks ::: p.links,
            dlinks = newDlink :: p.dlinks, // Don't forget to insert the correct DecompLink
            flaws = undecomp ::: open ::: (p.flaws - und),
            reason = reasonString,
            ordering = new Ordering(newOrderings),
            history = new Record("decompose", highStep, reasonString) :: p.history,
            parent = p,
            stepCount = highStep)

          kid
      }

    }

  /**
   * this repair method is modified so that we can detect un-decomposed events after insertion
   *
   */
  def repairOpen(p: Plan, open: OpenCond, g: GlobalInfo): List[Plan] =
    {
      SimpleRepair.reuseActions(p, open, g) ::: SimpleRepair.closedWorld(p, open, g) ::: insertAction(p, open, g)
    }

  /**
   * Delegates to the insert action method in SimpleRepairs, and then see if the newly inserted action
   * is composite. If it is, we insert a un-decomposed flaw
   */
  def insertAction(p: Plan, open: OpenCond, g: GlobalInfo): List[Plan] =
    {
      val plans = SimpleRepair.insertAction(p, open, g)
      plans map {
        p =>
          val newStep = p.id2step(p.stepCount).get.asInstanceOf[DecompAction]
          //println("inserted step " + newStep)
          if (newStep.composite)
            p.copy(flaws = new UnDecomposed(newStep.id) :: p.flaws)
          else p
      }
    }

  /** Find all un-decomposed actions in the plan and add them as flaws
   * This method is currently not used. 
   */
  def detectDecompFlaws(p: DecompPlan): List[Flaw] =
    {
      var undecomposed = p.steps collect {
        case action: DecompAction if (action.composite &&
          (!p.dlinks.exists(dlink => dlink.parent == action.id))) =>
          action
      } map (_.id)

      var existing = p.flaws collect {
        case u: UnDecomposed => u.id
      }

      undecomposed filterNot { existing contains } map { id => new UnDecomposed(id) }
    }

  class DecompException(msg: String) extends Exception(msg)
}