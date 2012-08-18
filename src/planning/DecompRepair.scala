package planning
import variable._
import action._
import logging._
import structures._
import scala.collection.mutable.ListBuffer
import scala.collection.mutable.HashMap

/**
 * repair algorithms for decompositional partial order planner (Young & Pollack 1996)
 * TODO: Test these methods. Add reuse.
 */
object DecompRepair extends Logging {

  def selectFlaw(p: Plan, g: DecompGlobal): Flaw =
    {
      // fold left to find the flaw with the highest priority
      //(p.flaws.head /: p.flaws.tail)((x, y) => if (x.priority <= y.priority) x else y)
      var bestflaw = p.flaws.head
      var bestnum = numRefinements(bestflaw, p, g)

      val fitness = p.flaws.tail foreach { f =>
        val num = numRefinements(f, p, g)
        if (num < bestnum) {
          bestnum = num
          bestflaw = f
        }
        /*else if (num == bestnum) (bestflaw, f) match {
          // choose the most recent undecomposed event
          case (best: UnDecomposed, d: UnDecomposed) if (best.id < d.id) =>
            bestnum = num
            bestflaw = d
          case (best: OpenCond, op: OpenCond) if (best.id < op.id) =>
            // choose the most recent open condition (i.e. as the tie breaker)
            bestnum = num
            bestflaw = op
          case _ =>
        }*/
      }

      bestflaw
    }

  private def numRefinements(f: Flaw, p: Plan, g: DecompGlobal): Int = f match {
    case d: UnDecomposed =>
      val name = p.id2step(d.id).get.name
      val num = g.recipes.count(_.name == name)
      num
    case t: Threat => 3 // tentative: always three ways of fixing any threats
    case op: OpenCond =>
      var verb = op.condition.verb
      var num = 0

      if (verb != 'not)
        num = p.initialState().count(_.verb == verb) + g.actionTemplates.count(_.effects.exists(_.verb == verb)) +
          p.steps.count(_.effects.exists(_.verb == verb))
      else {
        verb = op.condition.termlist.head.asInstanceOf[Proposition].verb
        num = p.initialState().filter(_.verb == 'not).count(_.termlist.head.asInstanceOf[Proposition].verb == verb) +
          g.actionTemplates.count(_.effects.filter(_.verb == 'not).exists(_.termlist.head.asInstanceOf[Proposition].verb == verb)) +
          p.steps.count(_.effects.filter(_.verb == 'not).exists(_.termlist.head.asInstanceOf[Proposition].verb == verb))
      }

      num
  }

  def refine(g: DecompGlobal)(p: Plan): List[Plan] =
    {
      if (!p.isInstanceOf[DecompPlan])
        throw new DecompException("The plan is not an instance of DecompPlan")
      if (!g.isInstanceOf[DecompGlobal])
        throw new DecompException("The global object is not an instance of DecompGlobal")

      trace { "reparing plan " + p.id }

      val kids =
        selectFlaw(p, g) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            repairOpen(p, open, g)
          case threat: Threat =>
            trace("repairing: " + threat)
            // threats are repaired in the same old way
            SimpleRepair.repairThreat(p, threat, g)
          case undecomposed: UnDecomposed =>
            trace("repairing: " + undecomposed)
            repairUnDecomposed(undecomposed, p.asInstanceOf[DecompPlan], g.asInstanceOf[DecompGlobal])
          case _ => throw new Exception("A flaw with no known repairs! " + p.flaws)
        }
      p.children = kids

      trace {
        "new plans: " +
          kids.map {
            plan =>
              plan.toString + "\n" +
                plan.planString()
          }
      }

      debug {
        readLine()
      }

      kids
    }

  def repairUnDecomposed(und: UnDecomposed, p: DecompPlan, g: DecompGlobal): List[Plan] =
    decompose(und, p, g) ::: reuseActionAsPart(und, p, g)

  /**
   * reuse some existing actions in the plan as part of the decomposition
   *
   */
  private def reuseActionAsPart(und: UnDecomposed, p: DecompPlan, g: DecompGlobal): List[Plan] = {
    // find a decomposition recipe
    val parent = p.id2step(und.id).get
    val reasonString = "decomposing step " + und.id
    val recipes = g.recipes.filter(r => r.name == parent.name)

    // containing all generated plans:
    val allPlans = ListBuffer[Plan]()

    // iterate over all possible recipes    
    recipes map {
      recipe =>

        var highStep = p.stepCount
        val newSteps = for (s <- recipe.steps) yield {
          highStep += 1
          s.doubleInstantiate(highStep, p.ultimateParent(parent.id))
        }

        // reuse candidates for each instantiated step are in one list
        val candidates = newSteps.map {
          template =>

            if (template.name == "kill") {
              //logging.DebugInfo.setDebug()
              //println("matching: " + template)
            }

            /* the choices variable contains all possible choices for this particular step in the decomposition.
             * "false" denote the case where the step is inserted into the plan.
             * Other steps are steps that can unify with the decomposition, i.e. steps that we can reuse
             */
            val choices = false :: {
              p.steps filter (DecompReuse.canUnifyAction(_, template, parent, p, g))
            }

            /*println("matched : " + a.map(_ match {
              case ac: Action => p.binding.substVarsShortString(ac)
              case _ => ""
            }).mkString(",\n"))
            */
            choices
        }
        //println(candidates.mkString("\n"))

        var all = candidates(0).map(List(_))
        //var nextAll = ListBuffer[ListBuffer[Any]]()
        //var f: ListBuffer[ListBuffer[Any]] = null

        for (
          i <- 1 until candidates.length
        ) {
          val result = for (
            x <- candidates(i);
            item <- all
          ) yield (x :: item)

          all = result

        }

        // filter out the all-false configuration because that is the same as a new decomposition
        all = all filterNot (_ forall (_ == false))

        all map { config =>

          // Step 1: convert the config to a list of actions that are either reused or newly inserted
          locateActions(newSteps, parent, config.reverse, p, g) match {
            case Some(triple) =>

              val (children, insertedSteps, existingStepIds, newBind) = triple
              // existingStepIds is a Map from step ids in the decomposition recipes to real step ids in the plan

              // Step 2: Create the decompositional link
              val ids = children.map(_.id)
              val decompLink = new DecompLink(parent.id, ids)

              // Step 3.1 make sure the reuse steps' positions are compatible with the parent step
              val stepIds = p.steps.map(_.id)
              //val beforeParent = filter(p.ordering.orderedBefore(_, parent.id))
              //val afterParent = filter(p.ordering.orderedBefore(parent.id, _))
              var valid = existingStepIds.values.forall { id =>
                stepIds.forall { pid =>
                  ((!p.ordering.orderedBefore(pid, parent.id)) || p.ordering.possiblyBefore(pid, id)) &&
                    ((!p.ordering.orderedBefore(parent.id, pid)) || p.ordering.possiblyBefore(id, pid))
                }
              }

              // Step 3.2: Make sure the orderings in the recipe are valid
              var newOrderings = ListBuffer[(Int, Int)]()

              valid = valid && recipe.ordering.forall {
                case (x, y) if existingStepIds.keySet.contains(x) && existingStepIds.keySet.contains(y) =>
                  val curId1 = existingStepIds(x)
                  val curId2 = existingStepIds(y)
                  newOrderings += ((curId1, curId2)) // side effect: keep this ordering
                  //println("testing the ordering: " + (curId1, curId2))
                  p.ordering.possiblyBefore(curId1, curId2)
                case (x, y) if existingStepIds.keySet.contains(x) =>
                  val curId1 = existingStepIds(x)
                  val curId2 = newSteps(y).id
                  newOrderings += ((curId1, curId2)) // side effect: keep this ordering
                  true
                case (x, y) if existingStepIds.keySet.contains(y) =>
                  val curId1 = newSteps(x).id
                  val curId2 = existingStepIds(y)
                  newOrderings += ((curId1, curId2)) // side effect: keep this ordering
                  true
                case (x, y) =>
                  newOrderings += ((newSteps(x).id, newSteps(y).id)) // side effect: keep this ordering
                  true
              }

              if (valid) {
                val existingIds = existingStepIds.values.toArray
                val insertedIds = insertedSteps.map(_.id).toArray

                val newOrd = p.ordering.copy
                newOrd.inheritOrdering(parent.id, existingIds, insertedIds)
                for (pair <- newOrderings)
                  newOrd.addOrder(pair._1, pair._2)

                // compute new causal links
                val newLinks = findCausalLinks(recipe.links, children, existingStepIds, p.binding, p, g)
                //println("new links = " + newLinks.mkString(", "))
                val undecomp = insertedSteps filter { _.asInstanceOf[DecompAction].composite } map { s => new UnDecomposed(s.id) }

                val open = insertedSteps flatMap { step =>
                  val existing = newLinks filter { l => l.id2 == step.id } map { _.precondition }
                  val remainder = step.preconditions filterNot { existing contains }
                  remainder map { cond => new OpenCond(step.id, cond) }
                }

                // Step 4: produce the plan here.

                val kid = p.decompCopy(
                  id = g.newPlanID(),
                  steps = insertedSteps ::: p.steps,
                  links = newLinks ::: p.links,
                  dlinks = decompLink :: p.dlinks, // Don't forget to insert the correct DecompLink
                  flaws = undecomp ::: open ::: (p.flaws filterNot (_ == und)),
                  binding = newBind,
                  reason = reasonString,
                  ordering = newOrd,
                  history = new Record("decompose", highStep, reasonString) :: p.history,
                  parent = p,
                  stepCount = highStep)

                // detect threats caused by inserted actions
                val threats = findThreats(insertedSteps, newLinks, kid, g)

                if (threats.isEmpty) {
                  //println("result = " + kid.detailString())
                  allPlans += kid
                } else {
                  val withThreats = kid.decompCopy(flaws = threats ::: kid.flaws)
                  //println("result = " + withThreats.detailString())
                  allPlans += withThreats
                }
              }

            case None => //println("returned none")
          }
        }
    }

    //println(allPlans.map(_.detailString()).mkString("all plans: ", ",\n", ""))
    //System.exit(0)
    allPlans.toList
  }

  private def locateActions(newSteps: List[DecompAction], parent: Action, config: List[Any], p: Plan, g: GlobalInfo): Option[(List[Action], List[Action], HashMap[Int, Int], Binding)] =
    {
      var stepCount = p.stepCount
      var count = 0
      var insertedSteps = ListBuffer[Action]()
      var existingStepIds = HashMap[Int, Int]()
      var newBind = p.binding
      //println("config = " + config)
      //println("new steps = " + newSteps)

      val children = {

        for (i <- 0 until config.length) yield config(i) match {
          case false =>
            count += 1
            //TODO: ugly code below
            val s = newSteps(i).doubleInstantiate(stepCount + count, p.asInstanceOf[DecompPlan].ultimateParent(parent.id))
            insertedSteps += s
            s
          case actual: Action =>
            val shadowed = newSteps(i)

            //println("i = " + i + " actual = " + actual.toString() + " shadowed = " + shadowed.toString)

            // check if these steps are compatible with corresponding steps
            // resulting in a new binding that synchronize the two            
            val opt = DecompReuse.directUnifyAction(actual, shadowed, parent, newBind, p, g)
            if (opt.isEmpty) {
              //println("unable to unify")
              return None
            } else
              newBind = opt.get

            existingStepIds += ((i, actual.id))
            actual
        }
      }.toList

      //      println("children = " + children.map(c => c.id + ": " + c))
      //      println("insertedSteps = " + insertedSteps.map(c => c.id + ": " + c))
      //      println("existing mapping = " + existingStepIds)
      Some((children, insertedSteps.toList, existingStepIds, newBind))
    }

  private def findCausalLinks(recipeLinks: List[Link], children: List[Action], existingStepIds: HashMap[Int, Int],
    bind: Binding, p: Plan, g: GlobalInfo): List[Link] = {
    recipeLinks map {
      fakeLink =>
        val step1 = children(fakeLink.id1)
        val step2 = children(fakeLink.id2)
        val step1IsOld = existingStepIds.keySet.contains(fakeLink.id1)
        val step2IsOld = existingStepIds.keySet.contains(fakeLink.id2)
        //println("step 1" + step1 + bind.substVarsShortString(step1))
        //println("step 2" + step2 + bind.substVarsShortString(step2))
        if (step1IsOld && (!step2IsOld)) {
          // step 1 (the earlier step) is old. One of its effect must unify with the link condition
          val effect = step1.effects.find(bind.canUnify(_, fakeLink.precondition, g)).get
          new Link(step1.id, step2.id, fakeLink.precondition, effect)
        } else if ((!step1IsOld) && step2IsOld) {
          // step 2 (the later step) is old. One of its preconditions must unify with the link condition
          //println("precond = " + step1.preconditions.map(bind.substVars))
          //println("effect = " + fakeLink.effect)
          val precond = step2.preconditions.find(bind.canUnify(_, fakeLink.effect, g)).get
          new Link(step1.id, step2.id, precond, fakeLink.effect)
        } else if (step1IsOld && step2IsOld) {
          val precond = step2.preconditions.find(bind.canUnify(_, fakeLink.effect, g)).get
          val effect = step1.effects.find(bind.canUnify(_, fakeLink.precondition, g)).get
          new Link(step1.id, step2.id, precond, effect)
        } else {
          // they are both new. This is the simplest case
          new Link(step1.id, step2.id, fakeLink.effect, fakeLink.effect)
        }
    }
  }

  private def findThreats(newSteps: List[Action], newLinks: List[Link], plan: Plan, g: GlobalInfo): List[Threat] =
    {
      newSteps.flatMap { SimpleRepair.detectThreats(_, plan, g) } :::
        newLinks.flatMap { SimpleRepair.detectThreats(_, plan, g) }
    }

  private def decompose(und: UnDecomposed, p: DecompPlan, g: DecompGlobal): List[Plan] = {

    // find a decomposition recipe
    val parent = p.id2step(und.id).get
    val reasonString = "decomposing step " + und.id
    val recipes = g.recipes.filter(r => r.name == parent.name)
    // decompose it
    recipes map {
      recipe =>

        var highStep = p.stepCount
        val newSteps = for (s <- recipe.steps) yield {
          highStep += 1
          s.doubleInstantiate(highStep, p.ultimateParent(parent.id))
        }

        val newLinks = recipe.links map {
          link =>
            val id1 = newSteps(link.id1).id
            val id2 = newSteps(link.id2).id
            val cond = link.precondition instantiate parent.id
            new Link(id1, id2, cond, cond)
        }

        val newOrd = p.ordering.copy()
        newOrd.inheritOrdering(parent.id, Array[Int](), newSteps.map(_.id).toArray)

        recipe.ordering foreach {
          order: (Int, Int) =>
            val id1 = newSteps(order._1).id
            val id2 = newSteps(order._2).id
            newOrd.addOrder(id1, id2)
        }
        //println("*****new orderings: " + (newOrderings -- p.ordering.list))

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
          flaws = undecomp ::: open ::: (p.flaws filterNot (_ == und)),
          reason = reasonString,
          ordering = newOrd,
          history = new Record("decompose", highStep, reasonString) :: p.history,
          parent = p,
          stepCount = highStep)

        val threats = findThreats(newSteps, newLinks, kid, g)

        if (threats.isEmpty) kid
        else {
          kid.decompCopy(flaws = threats ::: kid.flaws)
        }

    }

  }

  /**
   * this repair method is modified so that we can detect un-decomposed events after insertion
   *
   */
  private[planning] def repairOpen(p: Plan, open: OpenCond, g: GlobalInfo): List[Plan] =
    {
      SimpleRepair.reuseActions(p, open, g) ::: SimpleRepair.closedWorld(p, open, g) ::: insertAction(p, open, g)
    }

  /**
   * Delegates to the insert action method in SimpleRepairs, and then see if the newly inserted action
   * is composite. If it is, we insert a un-decomposed flaw
   */
  private[planning] def insertAction(p: Plan, open: OpenCond, g: GlobalInfo): List[Plan] =
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

  /**
   * Find all un-decomposed actions in the plan and add them as flaws
   * This method is currently not used.
   */
  private[planning] def detectDecompFlaws(p: DecompPlan): List[Flaw] =
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