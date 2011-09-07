package plan
import parsing.ActionParser
import parsing.ProblemParser
import variable._
import bestfirst._

object Main {
  def main(args: Array[String]) {
    //    val actions = ActionParser.readFile("./planfiles/actions.txt")
    //    val problem = ProblemParser.readFile("./planfiles/problem.txt")

    val actions = ActionParser.readFile("./planfiles/block1.act")
    val problem = ProblemParser.readFile("./planfiles/block1.prob")

    Global.init(actions, problem)
    var plan = Global.initPlan()
    //Global.debug = true
    val parameter = new SearchParameter(500)
    val bestfirst = new BestFirstSearch[Plan](List(plan), FlawRepair.refine _, complete _, eval _, parameter)

    try {
      val result = bestfirst.search()
      println("*************************")
      println("Found plan: " + result)
      println(result.planString())
      println(result.detailString())
    } catch {
      case e: Exception =>
        println("Search Failed: " + e.getMessage)

    } finally {
      println(bestfirst.stats)
    }

    //    println(plan.detailString)
    //    var plans = FlawRepair.refine(plan)
    //    println("*********************************")
    //    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }
    //
    //    plans = FlawRepair.refine(plans(0))
    //    println("*********************************")
    //    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }
  }

  def complete(p: Plan): Boolean = p.flaws == Nil

  def eval(p: Plan): Double =
    {
      if (p.flaws == Nil) 0
      else
        p.flaws.length + p.steps.length
    }

}