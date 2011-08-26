package plan
import variable._
object FlawRepair {

  def refine(p: Plan): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            repairOpen(p, open)
          case threat: Threat =>
            repairThreat(p, threat)
          case _ => throw new Exception("A flaw with no known repairs! " + p.flaws)
        }
      p.children = kids
      kids
    }

  def repairThreat(p: Plan, threat: Threat): List[Plan] =
    {
      promote(p, threat.id, threat.condition.id1) :::
        demote(p, threat.id, threat.condition.id2)
    }

  def repairOpen(p: Plan, open: OpenCond): List[Plan] =
    {
      reuseActions(p, open) ::: closedWorld(p, open) ::: insertAction(p, open)
    }

  def selectFlaw(p: Plan): Flaw =
    {
      // fold left to find the flaw with the highest priority
      (p.flaws.head /: p.flaws.tail)((x, y) => if (x.priority <= y.priority) x else y)
    }

  def insertAction(p: Plan, open: OpenCond): List[Plan] =
    {
      var kids = List[Plan]()
      val init = p.steps.find(_.id == 0).get.effects // initial state
      val highStep = p.stepCount + 1

      Global.actionTemplates foreach {
        template =>
          template.effects foreach {
            effect =>
              if (effect.equalsIgnoreVars(open.condition)) // filtering obviously impossible effects
              {
                // instantiate this effect
                val ie = effect.instantiate(highStep)
                // must respect constraints from both steps
                val allConstraints = (p.id2step(open.id) map (_.constraints) getOrElse(Nil)) ::: template.constraints  
                
                println("trying effect: " + ie)
                p.binding.unify(ie, open.condition, allConstraints, init) match {
                  case Some(newbind: Binding) =>
                    var newordering =
                      if (open.id == Global.GOAL_ID)
                        List(((highStep, open.id)), ((0, highStep)))
                      else List(((highStep, open.id)), ((0, highStep)), (highStep, Global.GOAL_ID))

                    val kid = p.copy(
                      steps = template.instantiate(highStep) :: p.steps,
                      links = new Link(highStep, open.id, open.condition) :: p.links,
                      binding = newbind,
                      flaws = p.flaws - open,
                      ordering = new Ordering(newordering ::: p.ordering.list),
                      reason = "Inserted action " + highStep + " to establish " + open.condition,
                      parent = p)

                    kids = detectThreats(kid) ::: kids
                  case None =>
                }
              }
          }
      }

      kids
    }

  def detectThreats(p: Plan): List[Plan] =
    {
      // TODO: search for new flaws threats here. 
      List[Plan](p)
    }
  //  
  def reuseActions(p: Plan, open: OpenCond): List[Plan] =
    {
      var kids = List[Plan]()
      val init = p.steps.find(_.id == 0).get.effects // initial state
      val possibleActions = p.ordering.possiblyBefore(open.id) map { p.id2step(_) } filterNot { _.isEmpty } map { _.get }
      possibleActions foreach {
        step =>
          val stepId = step.id
          step.effects foreach {
            effect =>
              if (p.binding.substVars(effect).equalsIgnoreVars(open.condition)) // filtering obviously impossible effects
              {
                println("trying effect: " + effect)
                p.binding.unify(effect, open.condition, step.constraints, init) match {
                  case Some(newbind: Binding) =>
                    var newordering = List(((stepId, open.id)))

                    val kid = p.copy(
                      links = new Link(stepId, open.id, open.condition) :: p.links,
                      binding = newbind,
                      flaws = p.flaws - open,
                      ordering = new Ordering(newordering ::: p.ordering.list),
                      reason = "Reused action " + stepId + " to establish " + open.condition,
                      parent = p)
                    kids = detectThreats(kid) ::: kids
                  case None =>
                }
              }
          }
      }
      kids
    }

  def closedWorld(p: Plan, open: OpenCond): List[Plan] =
    {
    // needs to check if this implicity conversion would work  
    if (open.condition.verb != 'not) return List[Plan]()
    
    val positiveP = open.condition.termlist(0)
    
    Nil
      
    }

  def promote(p: Plan, promoted: Int, top: Int): List[Plan] =
    {
      Nil
    }

  def demote(p: Plan, demoted: Int, bottom: Int): List[Plan] =
    {
      Nil
    }
//  private def bindInNewPlan(p: Plan, step: plan.Action, effect: variable.Proposition, open: OpenCond, init: List[variable.Proposition]): Unit = 
//    {  val stepId = step.id
//    if (p.binding.substVars(effect).equalsIgnoreVars(open.condition)) // filtering obviously impossible effects
//    {
//      println("trying effect: " + effect)
//      p.binding.unify(effect, open.condition, step.constraints, init) match {
//        case Some(newbind: Binding) =>
//          var newordering = List(((stepId, open.id)))
//
//          val kid = p.copy(
//            links = new Link(stepId, open.id, open.condition) :: p.links,
//            binding = newbind,
//            flaws = p.flaws - open,
//            ordering = new Ordering(newordering ::: p.ordering.list),
//            reason = "Reused action " + stepId + " to establish " + open.condition,
//            parent = p)
//          kids = detectThreats(kid) ::: kids
//        case None =>
//      }
//    }
//  }
}