package planning
import variable._
import logging._
import analogy._
import action._
import structures._
/* Fix at a later time - Albert 23 July 2012
 * 

object DoraFlawRepair extends Logging {

  def refine(p: Plan, g:GadgetGlobal): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            repairOpen(p, open, g)
          case threat: Threat =>
            trace("repairing: " + threat)
            SimpleRepair.repairThreat(p, threat, g)
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


  def repairOpen(p: Plan, open: OpenCond, g:GadgetGlobal): List[Plan] =
    {
      // no closed world. you have to insert it into the initial state
      insertIntoInitial(p, open) ::: projectAction(p, open) ::: transformExistingAction(p, open) :::
        SimpleRepair.reuseActions(p, open, g) ::: SimpleRepair.insertAction(p, open, g) // the second line are old methods
    }

  def selectFlaw(p: Plan): Flaw =
    {
      // fold left to find the flaw with the highest priority
      (p.flaws.head /: p.flaws.tail)((x, y) => if (x.priority <= y.priority) x else y)
    }

  def insertIntoInitial(p: Plan, open: OpenCond): List[Plan] =
    {

      List()
    }

  def projectAction(p: Plan, open: OpenCond, g:GadgetGlobal): List[Plan] =
    {
      // establish matching
      val protoActions = findProjectee(g.prototype, p, open)
      // make or find the action after projection
      debug { "projected action = " + protoActions }

      val projectedAction = protoActions flatMap { x => projectAction(p, open, x._1, x._2, x._3) }
      // insert the action into the plan
      projectedAction
    }

  def transformExistingAction(p: Plan, open: OpenCond): List[Plan] =
    {

      List()
    }

  protected def projectAction(plan: Plan, open: OpenCond, action: Action, effect: Proposition, fitness: Double): List[Plan] =
    {
      if (action.actor.pType == "Person") {
        analogicallyReplace(plan, action, effect, open)
      } else {
        analogicallyTransform(plan, action, effect, open, fitness)
      }
    }

  protected def analogicallyReplace(plan: Plan, action: Action, effect: Proposition, open: OpenCond, g:GadgetGlobal): List[Plan] =
    {
      var kids = List[Plan]()
      val init = plan.initialState
      val highStep = plan.stepCount + 1
      val temp = g.actionTemplates map
        {
          // this line ensures the inserted action has minimal resemblance to the action projected
          template => (template, AnalogyEngine.evalAnalogy(template, action))
        }

      debug { temp.mkString("remaining: ", ", ", "") }

      temp filter { _._2 > AnalogyEngine.goodThreshold } foreach {
        x =>
          val template = x._1
          val analogicalFitness = x._2
          // instantiate this template
          val newStep = template.instantiate(highStep)
          debug { "attempting " + newStep + " " + open }
          val neqs = newStep.constraints filter { _.verb == 'neq }
          val neqbind = plan.binding.addNeqs(neqs)
          newStep.effects foreach {
            effect =>
              if (neqbind.canUnify(effect, open.condition)) // filtering obviously impossible effects
              {
                // must respect constraints from both steps
                val allConstraints = ((plan.id2step(open.id) map (_.constraints) getOrElse (Nil)) ::: (newStep.constraints)) filterNot (_.verb == 'neq)

                debug("trying inserting step " + newStep + " with effect: " + effect + " for " + open.condition)
                neqbind.directUnify(effect, open.condition, allConstraints, init) match {
                  case Some(newbind: Binding) =>
                    debug("analogical unification succeeds")

                    var newordering =
                      if (open.id == GlobalInfo.GOAL_ID)
                        Set(((highStep, open.id)), ((0, highStep)))
                      else Set(((highStep, open.id)), ((0, highStep)), (highStep, GlobalInfo.GOAL_ID))

                    val newLink = new Link(highStep, open.id, effect, open.condition)

                    val reasonString = "Analogically projected action " + action +
                      " to step " + highStep + " to establish " + open.condition

                    val kid = plan.copy(
                      id = Global.newPlanID(),
                      steps = newStep :: plan.steps,
                      links = newLink :: plan.links,
                      binding = newbind,
                      flaws = newStep.preconditions.map { cond => new OpenCond(highStep, cond) } ::: (plan.flaws - open),
                      ordering = new Ordering(newordering ++ plan.ordering.list),
                      matchings = new ActionMatching(action, newStep) :: plan.matchings,
                      reason = reasonString,
                      history = new Record("AnaReplace", highStep, reasonString, analogicalFitness) :: plan.history,
                      parent = plan,
                      stepCount = highStep)

                    //kid.stepCount += 1

                    val threats = SimpleRepair.detectThreats(newStep, kid) ::: SimpleRepair.detectThreats(newLink, kid)

                    if (threats != Nil) // add detected threats into the plan
                      kids = kid.copy(flaws = threats ::: kid.flaws) :: kids
                    else kids = kid :: kids
                  case None => debug("unification failed")
                }
              }
          }
      }
      kids
    }

  protected def analogicallyTransform(plan: Plan, action: Action, effect: Proposition, open: OpenCond, fitness: Double, g:GadgetGlobal): List[Plan] =
    {
      
      var kids = List[Plan]()
      val init = plan.initialState
      val highStep = plan.stepCount + 1

      val newStep = action.instantiate(highStep)
      val newEffect = effect.instantiate(highStep)
      debug { "transforming " + newStep }
      val neqs = newStep.constraints filter { _.verb == 'neq }
      val neqbind = plan.binding.addNeqs(neqs)

      // must respect constraints from both steps
      val allConstraints = ((plan.id2step(open.id) map (_.constraints) getOrElse (Nil)) ::: (newStep.constraints)) filterNot (_.verb == 'neq)

      debug("trying inserting step " + newStep + " with effect: " + newEffect + " for " + open.condition)
      neqbind.analogicallyUnify(newEffect, open.condition, allConstraints, init) match {
        case Some(newbind: Binding) =>
          debug("unification succeeds")
          var newordering =
            if (open.id == Constants.GOAL_ID)
              Set(((highStep, open.id)), ((0, highStep)))
            else Set(((highStep, open.id)), ((0, highStep)), (highStep, GlobalInfo.GOAL_ID))

          val newLink = new Link(highStep, open.id, newEffect, open.condition)

          val reasonString = "Analogically transformed action " + highStep + " to establish " + open.condition
          var kid = plan.copy(
            id = GadgetGlobal.newPlanID(),
            steps = newStep :: plan.steps,
            links = newLink :: plan.links,
            binding = newbind,
            flaws = newStep.preconditions.map { p => new OpenCond(highStep, p) } ::: (plan.flaws - open),
            ordering = new Ordering(newordering ++ plan.ordering.list),
            reason = reasonString,
            history = new Record("AnaTransform", highStep, reasonString, fitness) :: plan.history,
            parent = plan,
            stepCount = highStep)

          //kid.stepCount += 1

          val threats = SimpleRepair.detectThreats(newStep, kid) ::: SimpleRepair.detectThreats(newLink, kid)

          if (threats != Nil) // add detected threats into the plan
            List(kid.copy(flaws = threats ::: kid.flaws))
          else List(kid)

        case None =>
          debug("unification failed")
          List()
      }
    }

  // returns the action, the effect of the action that matches the open precond, and the fitness of the analogy
  protected def findProjectee(prototype: Plan, gadget: GadgetPlan, open: OpenCond, g:GadgetGlobal): List[(Action, Proposition, Double)] =
    {
      val realCondition = gadget.binding.substVars(open.condition)

      if (open.id == Constants.GOAL_ID) {
        debug {
          "projecting for a goal condition"
        }
        // look for incoming links into the goal state of the prototype plan
        val goalLinks = prototype.links.filter(link => link.id2 == Constants.GOAL_ID)
        // compute the analogy values
        val analogyValues = goalLinks map { link =>
          AnalogyEngine.evalAnalogy(prototype.binding.substVars(link.precondition),
            realCondition)
        }

        // filter out the bad analogies
        val candidates = goalLinks zip analogyValues filter { x => x._2 > AnalogyEngine.goodThreshold }
        // convert the links to actions which extend the link
        candidates map { x => (prototype.id2step(x._1.id1).get, x._1.effect, x._2) }
      } else {
        // look for any correspondence between the action with the open precond
        // and action in the prototype frame
        debug {
          "projecting for a non-goal condition"
        }
        val gadgetAction = gadget.id2step(open.id).get
        debug {
          var a = gadget.matchings.mkString(", ")
          a += "\n" + gadgetAction.toString
          gadget.matchings.foreach { x =>
            if (x.isInstanceOf[ActionMatching])
              a += "\n" + x.asInstanceOf[ActionMatching].gadget == gadgetAction
          }
          a
        }
        val actions = gadget.matchings.filter {
          _ match {
            // find the correspondence to the gadget action we care about
            case am: ActionMatching => am.gadget == gadgetAction
            case _ => false
          }
        }.map {
          // this is the actual action from the prototype frame
          _.asInstanceOf[ActionMatching].prototype
        }

        // now find the incoming links of that action that corresponds to the current open precondition

        actions map { action =>
          val incomingLinks = prototype.links.filter(_.id2 == action.id)
          // use the first effect as the worst situation
          var bestLink: Link = incomingLinks(0)
          var best: Double = AnalogyEngine.evalAnalogy(prototype.binding.substVars(bestLink.precondition), realCondition)

          incomingLinks.tail foreach { link =>
            val realEffect = prototype.binding.substVars(link.precondition)
            val fitness = AnalogyEngine.evalAnalogy(realEffect, realCondition)
            if (best < fitness) {
              best = fitness
              bestLink = link
            }
          }
          
          (prototype.id2step(bestLink.id1).get, bestLink.precondition, best)
        }

        // other actions that have analogous effects
        /*val otherActions = prototype.steps flatMap {
        step => step.effects.exisprototype.binding.substVars()
      }*/
      }

    }

}
*/