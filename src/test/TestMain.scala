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
    val actions = ActionParser.readFile("./planfiles/test1.act")
    val problem = ProblemParser.readFile("./planfiles/test1.prob")

    Global.init(actions, problem)
    var plan = Global.initPlan()
    var plans = List(plan)
    for(i <- 0 to 7)
    {
      println("****")
      println(plans(0).planString())
      plans = FlawRepair.refine(plans(0)) 
    }
    println(plans(0))
    plans.foreach(x => println("\n"+x.detailString()+"\n"))
    println(plans(0).binding)
    
    plans = FlawRepair.refine(plans(0)) 
    plans.foreach(x => println("\n"+x.detailString()+"\n"))
  }
}






