package plan
import variable._
object FlawRepair {

  def refine(p: Plan): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            println("repairing: " + open)
            repairOpen(p, open)
          case threat: Threat =>
            println("repairing: " + threat)
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
          // instantiate this template
          val newStep = template.instantiate(highStep)
          val neqs = newStep.constraints filter { _.verb == 'neq }
          val neqbind = p.binding.addNeqs(neqs)
          newStep.effects foreach {
            effect =>
              if (neqbind.canUnify(effect, open.condition)) // filtering obviously impossible effects
              {
                // must respect constraints from both steps
                val allConstraints = (p.id2step(open.id) map (_.constraints) getOrElse (Nil)) ::: (newStep.constraints filterNot { neqs contains (_) })

                println("trying effect: " + effect)
                neqbind.unify(effect, open.condition, allConstraints, init) match {
                  case Some(newbind: Binding) =>
                    var newordering =
                      if (open.id == Global.GOAL_ID)
                        List(((highStep, open.id)), ((0, highStep)))
                      else List(((highStep, open.id)), ((0, highStep)), (highStep, Global.GOAL_ID))

                    val newLink = new Link(highStep, open.id, open.condition)
                    val kid = p.copy(
                      steps = newStep :: p.steps,
                      links = newLink :: p.links,
                      binding = newbind,
                      flaws = newStep.preconditions.map { p => new OpenCond(highStep, p) } ::: (p.flaws - open),
                      ordering = new Ordering(newordering ::: p.ordering.list),
                      reason = "Inserted action " + highStep + " to establish " + open.condition,
                      parent = p)

                    kid.stepCount += 1

                    val threats = detectThreats(newStep, kid) ::: detectThreats(newLink, kid)
                    
                    if (threats != Nil) // add detected threats into the plan
                      kids = kid.copy(flaws = threats ::: kid.flaws) :: kids
                    else kids = kid :: kids
                    
                  case None =>
                }
              }
          }
      }

      kids
    }

  def detectThreats(step: Action, p: Plan): List[Threat] =
    {
      // TODO: Test this method
      val before = p.ordering.possiblyBefore(step.id)
      val after = p.ordering.possiblyAfter(step.id)

      val threats = p.links filter { l =>
        // tests three conditions of causal threats
        before.contains(l.id1) && after.contains(l.id2) &&
          {
            step.effects exists { e =>
              val negated = e.negate // compute a negated proposition
              // make use of lazy evaluation to save computation
              p.binding.canEqual(negated, l.condition) && p.binding.canUnify(negated, l.condition)
            }
          }
      } map { l =>
        // make the threats
        new Threat(step.id, l)
      }
      threats
    }

  def detectThreats(newlink: Link, p: Plan): List[Threat] =
    {
      // TODO: Test this method
      val possible = p.ordering.possiblyBefore(newlink.id2)

      val threats = p.steps filter { step =>
        // tests three conditions of causal threats
        possible.contains(step.id) &&
          {
            step.effects exists { e =>
              val negated = e.negate // compute a negated proposition
              // make use of lazy evaluation to save computation
              p.binding.canEqual(negated, newlink.condition) && p.binding.canUnify(negated, newlink.condition)
            }
          }
      } map { step =>
        // make the threats
        new Threat(step.id, newlink)
      }
      threats
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
                println("trying effect: " + stepId + ": "+ effect)
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

                    kids = kid :: kids
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
      if (open.condition.verb != 'not) return Nil
      val condition = open.condition.termlist(0).asInstanceOf[Proposition]
      val init = p.initialState
      var bindings = List(p.binding)
      for (icond <- init) {
        bindings = bindings flatMap { x => x.separate(condition, icond) }
      }

      bindings map { bind =>
        p.copy(
          links = new Link(0, open.id, open.condition) :: p.links,
          binding = bind,
          flaws = p.flaws - open,
          reason = "Closed World Assumption: " + open.condition,
          parent = p)
      }

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