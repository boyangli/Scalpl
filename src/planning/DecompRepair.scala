package planning
import variable._
import action._
import logging._
import structures._

/**
 * repair algorithms for decompositional partial order planner (Young & Pollack 1996)
 *
 */
object DecompRepair extends Logging {

  def refine(g:DecompGlobal)(p: Plan): List[Plan] =
    {
      val kids =
        selectFlaw(p) match {
          case open: OpenCond =>
            trace("repairing: " + open)
            // open conditions are repaired in the same old way
            SimpleRepair.repairOpen(p, open, g)
          case threat: Threat =>
            trace("repairing: " + threat)
            // threats are repaired in the same old way
            SimpleRepair.repairThreat(p, threat, g)
          case undecomposed: UnDecomposed =>
            trace("repairing: " + undecomposed)
            repairUnDecomposed(p, undecomposed)
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

  def selectFlaw(p: Plan): Flaw =
    {
      // fold left to find the flaw with the highest priority
      (p.flaws.head /: p.flaws.tail)((x, y) => if (x.priority <= y.priority) x else y)
    }
  
  def repairUnDecomposed(p:Plan, und:UnDecomposed): List[Plan] = 
  {
    // find a decomposition recipe
    // decompose it
    // insert the correct DecompLink
    
    Nil
  }
}