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
      if (verifyThreat(threat, p)) {
        // the threat is still valid
        //var answer = List[Plan]()
        // first option: separate the two conflicting conditions
        val separated = p.binding.separate(threat.effect.negate, threat.threatened.condition) map {
          newbind =>
            p.copy(
              binding = newbind,
              flaws = p.flaws - threat,
              reason = "separating " + threat.effect + " and " + threat.threatened.condition,
              parent = p)
        }
        // second option: unify the two and make use of promotion and demotion
        // id2step returns an option. directly get from the options because 
        // they really should exist
        val threatStep = p.id2step(threat.id).get
        val step1 = p.id2step(threat.threatened.id1).get
        val step2 = p.id2step(threat.threatened.id2).get

        val constraints = threatStep.pureConstraints ::: step1.pureConstraints ::: step2.pureConstraints
        val newbind = p.binding.unify(threat.effect.negate, threat.threatened.condition, constraints, p.initialState)
        val unified = newbind match {
          case Some(nbind) => // unification successful
            promote(p, threat, nbind).toList :::
              demote(p, threat, nbind).toList
          case None => Nil
          // unification impossible. not need to demote or promote.
          // separation must be possible. but it has been done earlier
        }
        // finally, link results of the two options
        unified ::: separated
      } else {
        // the threat is not valid anymore
        // just ignore it
        val newplan = p.copy(
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

                println("trying inserting step " + newStep + " with effect: " + effect + " for " + open.condition)
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
                      parent = p,
                      stepCount = highStep)

                    //kid.stepCount += 1

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

  def verifyThreat(threat: Threat, p: Plan): Boolean =
    {
      val stepid = threat.id
      val link = threat.threatened
      val possible = p.ordering.possiblyBefore(link.id2)
      possible.contains(stepid) && p.binding.canUnify(threat.effect.negate, link.condition)
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
            p.binding.canEqual(negated, l.condition) && p.binding.canUnify(negated, l.condition)
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
            p.binding.canEqual(negated, newlink.condition) && p.binding.canUnify(negated, newlink.condition)
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
      val possibleActions = p.ordering.possiblyBefore(open.id) map { p.id2step(_) } filterNot { _.isEmpty } map { _.get }
      possibleActions foreach {
        step =>
          val stepId = step.id
          step.effects foreach {
            effect =>
              if (p.binding.substVars(effect).equalsIgnoreVars(open.condition)) // filtering obviously impossible effects
              {
                println("trying effect: " + stepId + ": " + effect + " for " + open.condition)
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

  def promote(p: Plan, threat: Threat, bind: Binding): Option[Plan] =
    {
      val promoted = threat.id
      val top = threat.threatened.id1
      if (p.ordering.possiblyBefore(top).contains(promoted))
        new Some(p.copy(
          ordering = p.ordering + ((promoted, top)),
          binding = bind,
          flaws = p.flaws - threat,
          reason = "promoting step " + promoted + " before " + top,
          parent = p))
      else None
    }

  def demote(p: Plan, threat: Threat, bind: Binding): Option[Plan] =
    {
      val demoted = threat.id
      val bottom = threat.threatened.id2
      if (p.ordering.possiblyAfter(bottom).contains(demoted))
        new Some(p.copy(
          ordering = p.ordering + ((bottom, demoted)),
          binding = bind,
          flaws = p.flaws - threat,
          reason = "demoting step " + demoted + " after " + bottom,
          parent = p))
      else None
    }
}