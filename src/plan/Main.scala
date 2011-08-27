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
    var plan = Global.initPlan()
    
    println(plan.detailString)
    var plans = FlawRepair.refine(plan)
    println("*********************************")
    plans foreach {x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n")}

    
    plans = FlawRepair.refine(plans(0))
    println("*********************************")
    plans foreach {x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n")}
  }
  
  
}