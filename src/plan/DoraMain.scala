package plan
import parsing._

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
    Global.init(allActions, problem, protoPlan)
    
    println(Global.prototype.detailString())
    println(allActions)
  }
  
  
}