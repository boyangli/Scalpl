package plan
import variable._
import logging._
import analogy._

object DoraFlawRepair extends Logging {

  def refine(p: Plan): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            repairOpen(p, open)
          case threat: Threat =>
            trace("repairing: " + threat)
            FlawRepair.repairThreat(p, threat)
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

  def repairOpen(p: Plan, open: OpenCond): List[Plan] =
    {
      // no closed world. you have to insert it into the initial state
      insertIntoInitial(p, open) ::: projectAction(p, open) ::: transformExistingAction(p, open) :::
        FlawRepair.reuseActions(p, open) ::: FlawRepair.insertAction(p, open) // the second line are old methods
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

  def projectAction(p: Plan, open: OpenCond): List[Plan] =
    {
		  // establish matching
    val protoAction = findProjectee(Global.prototype, p, open)
    // make or find the action after projection
    val projectedAction = projectAction(protoAction)
    // insert the action into the plan
      List()
    }

  def transformExistingAction(p: Plan, open: OpenCond): List[Plan] =
    {

      List()
    }
  
  def findProjectee(prototype:Plan, gadget:Plan, open:OpenCond):List[(Action, Proposition, Double)] =
  {
    if (open.id == Global.GOAL_ID)
    {
      // look for incoming links into the goal state of the prototype plan
      val goalLinks = prototype.links.filter(link => link.id2 == Global.GOAL_ID)
      // compute the analogy values
      val analogyValues = goalLinks.map{link => AnalogyEngine.evalAnalogy(link.precondition, open.condition)}
      // filter out the bad analogies
      val candidates = goalLinks zip analogyValues filter { x => x._2 > AnalogyEngine.goodThreshold}
      // convert the links to actions which extend the link
      candidates map {x => (prototype.id2step(x._1.id1).get, x._1.effect, x._2)}
    }
    else
    {
      // look for any correspondence between the action with the open precond
      // and action in the prototype frame
      val gadgetAction = gadget.id2step(open.id) 
        
      val actions = gadget.matchings.filter{_ match {
        case am:ActionMatching => am.gadget == gadgetAction
        case _ => false
      }}.map {
        _.asInstanceOf[ActionMatching].prototype
      }
      
      actions map {m => 
      

  }

}