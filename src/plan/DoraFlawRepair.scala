package plan
import variable._
import logging._
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

      List()
    }

  def transformExistingAction(p: Plan, open: OpenCond): List[Plan] =
    {

      List()
    }

}