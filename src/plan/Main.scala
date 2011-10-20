package plan
import parsing.ActionParser
import parsing.ProblemParser
import variable._
import bestfirst._

object Main {
  def main(args: Array[String]) {
    //    val actions = ActionParser.readFile("./planfiles/actions.txt")
    //    val problem = ProblemParser.readFile("./planfiles/problem.txt")

    val actions = try {
      ActionParser.readFile("./planfiles/test3.act")
    } catch {
      case e: RuntimeException =>
        println("cannot parse action file: " + e.getMessage())
        return
    }
    val problem = try {
      ProblemParser.readFile("./planfiles/test3.prob")
    } catch {
      case e: RuntimeException =>
        println("cannot parse problem file: " + e.getMessage())
        return
    }
    Global.init(actions, problem)
    //Global.setDebug()
    var plan = Global.initPlan()
    //Global.debug = true
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
    val actions = ActionParser.readFile(actionFile)
    val problem = ProblemParser.readFile(problemFile)

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
      {
        val past = p.history.map {
          record => record.oper match
          {
            case "insert" => 2
            case "reuse" => 2
            case "closed-world" => 2
            case "promote" => 1
            case "demote" => 1
            case "separate" => 1            
          }
        }.foldLeft(0)((a, b) => a+b)
        
        val future = p.flaws.map
        {
          f => f match {
            case o:OpenCond => 2
            case t:Threat => 1
          }          
        }.foldLeft(0)((a, b) => a+b)
        
        past + future
      }
    }

}