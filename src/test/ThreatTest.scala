package test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import variable._
import variable.PopSymbol._
import plan._
import parsing._

class ThreatTest {

  @Test def plan1() {
    val actions = ActionParser.readFile("./planfiles/threat1actions.txt")
    val problem = ProblemParser.readFile("./planfiles/threat1prob.txt")

    Global.init(actions, problem)
    var plan = Global.initPlan()

    println(plan.detailString)
    var plans = FlawRepair.refine(plan)
    println("*********************************")
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }

    plans = FlawRepair.refine(plans(0))
    println("*********************************")
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }
  }
}


