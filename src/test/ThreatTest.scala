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

    plans = FlawRepair.refine(plans(0))
    //println("high step:" + plans(0).stepCount)
    println("*********************************")
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }

    plans = FlawRepair.refine(plans(0))
    //println("high step:" + plans(0).stepCount)
    println("*********************************")
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }

    plans = FlawRepair.refine(plans(0))
    println("*********************************")
    //println("high step:" + plans(0).stepCount)
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }

    plans = FlawRepair.refine(plans(0))
    println("*********************************")
    plans foreach { x => println("plan: \n" + x.detailString() + "\nbinding: " + x.binding + "\n\n") }

  }

  @Test def testPlanCopy() {
    val actions = ActionParser.readFile("./planfiles/threat1actions.txt")
    val problem = ProblemParser.readFile("./planfiles/threat1prob.txt")

    Global.init(actions, problem)
    var plan = Global.initPlan()
    
    plan = plan.copy(stepCount = 1)
    assertEquals(plan.stepCount, 1)
  }
}


