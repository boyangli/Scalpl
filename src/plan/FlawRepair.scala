package plan
import variable._
import logging._
object FlawRepair extends Logging {

  def refine(p: Plan): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            repairOpen(p, open)
          case threat: Threat =>
            trace("repairing: " + threat)
            repairThreat(p, threat)
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

  def repairThreat(p: Plan, threat: Threat): List[Plan] =
    {
      // first, check if the threat is still valid
      if (verifyThreat(threat, p)) {

        // first option: separate the two conflicting conditions
        val separated = p.binding.separate(threat.effect.negate, threat.threatened.precondition) map {
          newbind =>
            debug("Separated " + threat.effect.negate + " and " + threat.threatened.precondition)
            val reasonString = "separating " + threat.effect + " and " + threat.threatened.precondition
            p.copy(
              id = Global.newPlanID(),
              binding = newbind,
              flaws = p.flaws - threat,
              reason = reasonString,
              history = new Record("separate", threat.id, reasonString) :: p.history,
              parent = p)
        }
        // second option: unify the two and make use of promotion and demotion
        // id2step returns an option. directly get from the options because 
        // they really should exist
        val threatStep = p.id2step(threat.id).get
        val step1 = p.id2step(threat.threatened.id1).get
        val step2 = p.id2step(threat.threatened.id2).get

        /*
        val constraints = threatStep.constraints ::: step1.constraints ::: step2.constraints
        val neqs = constraints filter { _.verb == 'neq }
        val realConstraints = constraints filter { _.verb != 'neq }
        val neqbind = p.binding.addNeqs(neqs)
        */

        //here we do not have to add the neq constraints again.
        // those constraints are added to the binding when steps are added into the plan
        val constraints = threatStep.pureConstraints ::: step1.pureConstraints ::: step2.pureConstraints
        val newbind = p.binding.unify(threat.effect.negate, threat.threatened.precondition, constraints, p.initialState)
        val unified = newbind match {
          case Some(nbind) => // unification successful
            promote(p, threat, nbind).toList :::
              demote(p, threat, nbind).toList
          case None => Nil
          // unification impossible. not need to demote or promote.
          // separation must be possible. but it would have been done earlier, so we return nil here
        }
        // finally, link results of the two options
        unified ::: separated
      } else {
        // the threat is not valid anymore, just remove it
        // no history records necessary
        val newplan = p.copy(
          id = Global.newPlanID(),
          flaws = p.flaws - threat,
          reason = "threat " + threat + " is no longer valid",
          parent = p)
        List(newplan)
      }
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
      val init = p.initialState
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
                val allConstraints = ((p.id2step(open.id) map (_.constraints) getOrElse (Nil)) ::: (newStep.constraints)) filterNot (_.verb == 'neq)

                debug("trying inserting step " + newStep + " with effect: " + effect + " for " + open.condition)
                neqbind.directUnify(effect, open.condition, allConstraints, init) match {
                  case Some(newbind: Binding) =>
                    debug("unification succeeds")
                    var newordering =
                      if (open.id == Global.GOAL_ID)
                        Set(((highStep, open.id)), ((0, highStep)))
                      else Set(((highStep, open.id)), ((0, highStep)), (highStep, Global.GOAL_ID))

                    val newLink = new Link(highStep, open.id, effect, open.condition)

                    val reasonString = "Inserted action " + highStep + " to establish " + open.condition
                    val kid = p.copy(
                      id = Global.newPlanID(),
                      steps = newStep :: p.steps,
                      links = newLink :: p.links,
                      binding = newbind,
                      flaws = newStep.preconditions.map { p => new OpenCond(highStep, p) } ::: (p.flaws - open),
                      ordering = new Ordering(newordering ++ p.ordering.list),
                      reason = reasonString,
                      history = new Record("insert", highStep, reasonString) :: p.history,
                      parent = p,
                      stepCount = highStep)

                    //kid.stepCount += 1

                    val threats = detectThreats(newStep, kid) ::: detectThreats(newLink, kid)

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

 

  def verifyThreat(threat: Threat, p: Plan): Boolean =
    {
      val stepid = threat.id
      val link = threat.threatened
      val possible = p.ordering.possiblyBefore(link.id2)
      possible.contains(stepid) && p.binding.canUnify(threat.effect.negate, link.precondition)
    }

  /**
   * tests if the specified action contains effects that would threaten any links
   * in the plan
   */
  def detectThreats(step: Action, p: Plan): List[Threat] =
    {
      val before = p.ordering.possiblyBefore(step.id)
      val after = p.ordering.possiblyAfter(step.id)

      val threats = p.links filter { l =>
        // tests the position of threatened link
        before.contains(l.id1) && after.contains(l.id2)
      } flatMap
        { l =>
          step.effects filter { effect =>
            val negated = effect.negate // compute a negated proposition
            // make use of lazy evaluation to save computation
            p.binding.canEqual(negated, l.precondition) && p.binding.canUnify(negated, l.precondition)
          } map { effect =>
            new Threat(step.id, effect, l)
          }
        }
      threats
    }

  /**
   * tests if the specified link is threatened by any actions in the plan
   *
   */
  def detectThreats(newlink: Link, p: Plan): List[Threat] =
    {
      val possible = p.ordering.possiblyBefore(newlink.id2)

      val threats = p.steps filter { step =>
        // tests three conditions of causal threats
        possible.contains(step.id)
      } flatMap {
        step =>
          step.effects filter { effect =>
            val negated = effect.negate // compute a negated proposition
            // make use of lazy evaluation to save computation
            p.binding.canEqual(negated, newlink.precondition) && p.binding.canUnify(negated, newlink.precondition)
          } map {
            effect =>
              new Threat(step.id, effect, newlink)
          }
      }

      threats
    }

  def reuseActions(p: Plan, open: OpenCond): List[Plan] =
    {
      var kids = List[Plan]()
      val init = p.steps.find(_.id == 0).get.effects // initial state
      val possibleActions = p.ordering.possiblyBefore(open.id) flatMap { p.id2step(_) }
      possibleActions foreach {
        oldstep =>
          val stepId = oldstep.id

          oldstep.effects foreach {
            effect =>
              if (p.binding.canUnify(effect, open.condition)) // filtering obviously impossible effects
              {
                debug("trying to reuse effect: " + stepId + ": " + effect + " for " + open.condition)
                val constraints = (oldstep.constraints ::: p.id2step(open.id).map(_.constraints).getOrElse(Nil)) filterNot { _.verb == 'neq }

                p.binding.directUnify(effect, open.condition, constraints, init) match {
                  case Some(newbind: Binding) =>
                    debug("reuse succeeds")
                    var newOrdering = Set(((stepId, open.id)))
                    val newLink = new Link(stepId, open.id, effect, open.condition)
                    val reasonString = "Reused action " + stepId + " to establish " + open.condition
                    var kid = p.copy(
                      id = Global.newPlanID(),
                      links = newLink :: p.links,
                      binding = newbind,
                      flaws = p.flaws - open,
                      ordering = new Ordering(newOrdering ++ p.ordering.list),
                      reason = reasonString,
                      history = new Record("reuse", stepId, reasonString) :: p.history,
                      parent = p)

                    val threats = detectThreats(newLink, kid)
                    if (threats != Nil) // add detected threats into the plan
                      kid = kid.copy(flaws = threats ::: kid.flaws)

                    kids = kid :: kids
                  case None => debug("reuse failed")
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
        val newLink = new Link(0, open.id, Proposition('closedworld), open.condition)
        val reasonString = "Closed World Assumption: " + open.condition
        var kid =
          p.copy(
            id = Global.newPlanID(),
            links = newLink :: p.links,
            binding = bind,
            flaws = p.flaws - open,
            reason = reasonString,
            history = new Record("closed-world", open.id, reasonString) :: p.history,
            parent = p)
        val threats = detectThreats(newLink, kid)
        if (threats != Nil) // add detected threats into the plan
          kid = kid.copy(flaws = threats ::: kid.flaws)

        kid
      }
    }

  def promote(p: Plan, threat: Threat, bind: Binding): Option[Plan] =
    {
      val promoted = threat.id
      val top = threat.threatened.id1
      if (p.ordering.possiblyBefore(top).contains(promoted)) {
        debug { "promoted step " + promoted + " before " + top }
        val reasonString = "promoting step " + promoted + " before " + top
        new Some(p.copy(
          id = Global.newPlanID(),
          ordering = p.ordering + ((promoted, top)),
          binding = bind,
          flaws = p.flaws - threat,
          reason = reasonString,
          history = new Record("promote", promoted, reasonString) :: p.history,
          parent = p))
      } else None
    }

  def demote(p: Plan, threat: Threat, bind: Binding): Option[Plan] =
    {
      val demoted = threat.id
      val bottom = threat.threatened.id2
      if (p.ordering.possiblyAfter(bottom).contains(demoted)) {
        debug { "demoted step " + demoted + " before " + bottom }
        val reasonString = "demoting step " + demoted + " after " + bottom
        new Some(p.copy(
          id = Global.newPlanID(),
          ordering = p.ordering + ((bottom, demoted)),
          binding = bind,
          flaws = p.flaws - threat,
          reason = reasonString,
          history = new Record("demote", demoted, reasonString) :: p.history,
          parent = p))
      } else None
    }
}