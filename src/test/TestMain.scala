package test
import parsing.ActionParser
import parsing.ProblemParser
import variable._
import bestfirst._
import scala.util.parsing.combinator._
import variable._
import plan._

object TestMain {

  def main(args: Array[String]) {
    val actions = ActionParser.readFile("./planfiles/test2.act")
    val problem = ProblemParser.readFile("./planfiles/test2.prob")

    Global.init(actions, problem)
    var plan = Global.initPlan()
    Global.setDebug
    var plans = List(plan)
    plans = FlawRepair.refine(plan)
    plan = plans(0)
    println(plans.map(_.planString() + "\n").mkString("1st plans: ", "", "---end"))
    println(plan.planString())
    plans = FlawRepair.refine(plan)
    for (i <- 0 to 2) {
      println("****")
      plan = plans.last
      println(plan.planString())
      plans = FlawRepair.refine(plan)
    }
    println(plans(0))
    plans.foreach(x => println("\n" + x.detailString() + "\n"))
    println(plans(0).binding)

    plans = FlawRepair.refine(plans(0))
    plans.foreach(x => println("\n" + x.detailString() + "\n"))
  }
}






