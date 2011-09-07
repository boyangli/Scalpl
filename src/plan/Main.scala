package plan
import parsing.ActionParser
import parsing.ProblemParser
import variable._
import bestfirst._

object Main {
  def main(args: Array[String]) {
    //    val actions = ActionParser.readFile("./planfiles/actions.txt")
    //    val problem = ProblemParser.readFile("./planfiles/problem.txt")

    val actions = ActionParser.readFile("./planfiles/test1.act")
    val problem = ProblemParser.readFile("./planfiles/test1.prob")

    Global.init(actions, problem)
    var plan = Global.initPlan()
    val parameter = new SearchParameter(50)
    val bestfirst = new BestFirstSearch[Plan](List(plan), FlawRepair.refine _, complete _, eval _, parameter)

    try {
      val result = bestfirst.search()
      println("*************************")
      println("Found plan: " + result)
      println(result.planString())
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
      p.flaws.length + p.steps.length
    }

}