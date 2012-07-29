package test
import planning._
import structures._
import parsing._
import bestfirst._
import action._

object TestDecompPlanning extends App {

  val problemFile = "./planfiles/dpocl1.prob"
  val actionFile = "./planfiles/dpocl1.act"
  val decompFile = "./planfiles/dpocl1.decomp"

  val (problem, actions, recipes) = TotalParser.decompParse(problemFile, actionFile, decompFile)
  //val recipes = DecompParser.readFile(decompFile)
  val g = new DecompGlobal(actions, problem, recipes)
  var plan = g.initPlan()
  //Global.debug = true
  val parameter = new SearchParameter(500)
  val bestfirst = new BestFirstSearch[Plan](List(plan), DecompRepair.refine(g) _, complete _, eval _, parameter)
  /*
  g.actionTemplates.foreach { a =>
    val c = a.asInstanceOf[DecompAction].composite
    println(a.toString() + " composite = " + c +
      {
        if (c) g.recipes.find(_.name == a.name).get.toString()
        else ""
      })
  }
  */
  val first = bestfirst.search()

  println(first.detailString())
  /*
  for (i <- 0 to 2) {
    var plans = DecompRepair.refine(g)(plan)
    plans.foreach(plan => println(plan.toParseString() + "\nflaws: " + plan.flaws))
    plan = plans(0)
    println("*** next iteration ***")
  }
*/
  def complete(p: Plan): Boolean = p.flaws == Nil

  /* this is an A* heuristic. It takes into account historical costs and future estimates. 
   * although it is slower for simple problems, it is very beneficial for bigger problems. 
   * E.g. to solve Test 3 the planner visits more than 19000 plans with the simple heuristic,
   * but with the A* heuristic it only visits 9497 plans
   */
  def eval(p: Plan): Double =
    {
      if (p.flaws == Nil) 0
      else {
        val past = p.history.map {
          record =>
            record.oper match {
              case "insert" => 2
              case "reuse" => 1
              case "closed-world" => 1
              case "promote" => 1
              case "demote" => 1
              case "separate" => 1
              case "decompose" => 1
            }
        }.foldLeft(0)((a, b) => a + b)

        val future = p.flaws.map {
          f =>
            f match {
              case o: OpenCond => 1
              case t: Threat => 1
              case u: UnDecomposed => 1
            }
        }.foldLeft(0)((a, b) => a + b)

        past + future
      }
    }

}
