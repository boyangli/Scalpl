package planning
import parsing._
import structures._
import analogy._
import action._

/* fix at a later time
 * 
 
object DoraMain {

  def main(args:Array[String])
  {
    val (actions, protoPlan) = try {
      PlanParser.parse("./planfiles/toyphone.act", "./planfiles/toyphone.prob", "./planfiles/toyphone.plan")
    } catch {
      case e: RuntimeException =>
        println("cannot parse file: " + e.getMessage())
        e.printStackTrace()
        return
    }
    
    // this is the problem that the gadget is trying to solve
    val (problem, otherActions) = TotalParser.parse("./planfiles/fluphone.prob", "./planfiles/fluphone.act")
    
    val allActions:List[Action] = (actions ::: otherActions).distinct 
    GlobalInfo.init(allActions, problem, protoPlan)
    GlobalInfo.setDebug()
    var plan = GlobalInfo.initPlan()
    // insert the analogies
    val matchings = List[Matching]()
    
    //println("prototype: \n" + Global.prototype.detailString())
    //println("actions: \n" + allActions)
    //println("plans: \n" + plan.initialState() + "\n" + plan.goalState() + "\n " + plan.flaws)
    
    plan = DoraFlawRepair.refine(plan)(0)
    
    DoraFlawRepair.refine(plan) foreach {
      p => println(p.detailString())
      println(p.binding)
    }
    
  }
 } 
  */
