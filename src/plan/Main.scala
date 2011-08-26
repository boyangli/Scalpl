package plan
import parsing.ActionParser
import parsing.ProblemParser
import variable._

object Main {
  def main(args:Array[String])
  {
    val actions = ActionParser.readFile("./planfiles/actions.txt")
    val problem = ProblemParser.readFile("./planfiles/problem.txt")
    
    Global.init(actions, problem)
    val plan = Global.initPlan()
    
    println(plan.detailString)
    val p2 = FlawRepair.refine(plan)
    println("plan: \n" + p2(0).detailString())
    println('symbol == PopSymbol('symbol).symbol)
  }
  
  
}