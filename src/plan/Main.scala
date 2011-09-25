package plan
import parsing.TotalParser
import variable._
import bestfirst._

object Main {
  def main(args: Array[String]) {
    //    val actions = ActionParser.readFile("./planfiles/actions.txt")
    //    val problem = ProblemParser.readFile("./planfiles/problem.txt")

    val (problem, actions) = try {
      TotalParser.parse("./planfiles/test3.prob", "./planfiles/test3.act")
    } catch {
      case e: RuntimeException =>
        println("cannot parse file: " + e.getMessage())
        e.printStackTrace()
        return
    }

    Global.init(actions, problem)
    //Global.setDebug()
    var plan = Global.initPlan()
    //Global.setTrace()
    val parameter = new SearchParameter(50000)
    val bestfirst = new BestFirstSearch[Plan](List(plan), FlawRepair.refine _, complete _, eval _, parameter)

    try {
      val result = bestfirst.search()
      println("*************************")
      println("Found plan: " + result)
      println(result.planString())
      println(result.detailString())

      val t = result.links flatMap {
        link =>
          FlawRepair.detectThreats(link, result)
      }

      println(t)
    } catch {
      case e: Exception =>
        println("Search Failed: " + e.getMessage)

    } finally {
      println(bestfirst.stats)
    }

  }

  def plan(actionFile: String, problemFile: String): (Option[Plan], SearchStats) = {
    val (problem, actions) = TotalParser.parse(problemFile, actionFile)

    Global.init(actions, problem)
    var plan = Global.initPlan()
    //Global.debug = true
    val parameter = new SearchParameter(500)
    val bestfirst = new BestFirstSearch[Plan](List(plan), FlawRepair.refine _, complete _, eval _, parameter)

    val first =
      try {
        Some(bestfirst.search())
      } catch {
        case e: Exception =>
          println("Search Failed: " + e.getMessage)
          None
      }

    (first, bestfirst.stats)
  }

  def complete(p: Plan): Boolean = p.flaws == Nil

  def eval(p: Plan): Double =
    {
      if (p.flaws == Nil) 0
      else
        p.flaws.length + p.steps.length
    }

}