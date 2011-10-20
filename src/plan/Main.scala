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
      //TotalParser.parse("./planfiles/toyphone.prob", "./planfiles/toyphone.act")
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
//      println(result.planString())
      println(result.detailString())
      println("parsible:" + result.parsibleString())


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

  /* this is an A* heuristic. It takes into account historical costs and future estimates. 
   * although it is slower for simple problems, it is very beneficial for bigger problems. 
   * E.g. to solve Test 3 the planner visits more than 19000 plans with the simple heuristic,
   * but with the A* heuristic it only visits 9497 plans
   */
  def eval(p: Plan): Double =
    {
      if (p.flaws == Nil) 0
      else
      {
        val past = p.history.map {
          record => record.oper match
          {
            case "insert" => 2
            case "reuse" => 1
            case "closed-world" => 1
            case "promote" => 1
            case "demote" => 1
            case "separate" => 1            
          }
        }.foldLeft(0)((a, b) => a+b)
        
        val future = p.flaws.map
        {
          f => f match {
            case o:OpenCond => 1
            case t:Threat => 1
          }          
        }.foldLeft(0)((a, b) => a+b)
        
        past + future
      }
    }
  
  /* this is a simple heuristic that only considers the estimated distance to goal
   * This works well for small problems but is slow for large problems where the historical cost becomes significant.
   */
  def simpleEval(p: Plan): Double =
    {
      p.steps.length + p.flaws.length
    }

}