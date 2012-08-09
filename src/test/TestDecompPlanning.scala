package test
import planning._
import logging._
import structures._
import parsing._
import bestfirst._
import action._

object TestDecompPlanning {

  def main(args: Array[String]) {

    val problemFile = "./planfiles/game2.prob"
    val actionFile = "./planfiles/game2.act"
    val decompFile = "./planfiles/game2.decomp"

    /*
  val problemFile = "./planfiles/pharmacy.prob"
  val actionFile = "./planfiles/pharmacy.act"
  val decompFile = "./planfiles/pharmacy.decomp"
  */
    readLine()

    val (problem, actions, recipes) = TotalParser.decompParse(problemFile, actionFile, decompFile)

    val g = new DecompGlobal(actions, problem, recipes)
    //println(g.ontology)
    var plan = g.initPlan()
    //DebugInfo.setDebug()
    val parameter = new SearchParameter(100000)
    val bestfirst = new BestFirstSearch[Plan](List(plan), DecompRepair.refine(g) _, complete _, eval(g) _, parameter)
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
    println(bestfirst.stats)
  }
  //println(g.actionTemplates.map(a => a.name + ": effects = \n" + a.effects.mkString("\n")).mkString(",\n"))
  //println()
  //println(g.recipes.map(r => r.name + ": " + r.steps.map(_.effects).mkString(",")))

  //  for (i <- 0 to 1) {
  //    var plans = DecompRepair.refine(g)(plan)
  //    plans.foreach(plan => println(plan.toParseString() + "\nflaws: " + plan.flaws))
  //    plan = plans(0)
  //    println("*** next iteration ***")
  //  }
  //  
  //  println(plan.id2step(2).get.effects)

  def complete(p: Plan): Boolean = p.flaws == Nil

  /* this is an A* heuristic. It takes into account historical costs and future estimates. 
   * although it is slower for simple problems, it is very beneficial for bigger problems. 
   * E.g. to solve Test 3 the planner visits more than 19000 plans with the simple heuristic,
   * but with the A* heuristic it only visits 9497 plans
   */
  def eval(g: DecompGlobal)(p: Plan): Double =
    {
      if (p.flaws == Nil) 0
      else {
        val past = p.history.map {
          record =>
            record.oper match {
              case "insert" => 10
              case "reuse" => 10
              case "closed-world" => 10
              case "promote" => 5
              case "demote" => 5
              case "separate" => 5
              case "decompose" => 10
              case _ => 10
            }
        }.foldLeft(0)((a, b) => a + b)

        val future = p.flaws.map {
          f =>
            f match {
              case o: OpenCond => 10
              case t: Threat => 5
              case u: UnDecomposed =>
                val name = p.id2step(u.id).get.name
                g.recipes.filter(_.name == name).map { _.steps.size }.min * 10
            }
        }.foldLeft(0)((a, b) => a + b)
        /*
        val preferred = List(
          "(move warrior bar home)",
          "(move warrior home bar)",
          "(talk warrior uncle home)",
          "(die-from-injury uncle)",
          "(kill warrior thief sword bar)",
          "(loot-corpse warrior uncle sword home)",
          "(loot-corpse warrior thief secret-book bar)",
          "(kill warrior guard1 sword jail)")
         
        val disliked = List("(loot-corpse warrior guard1 ?a ?b)",
            "(move uncle ?a ?b)",
            "(move witch-doctor ?a ?b)",
            "(move guard ?a ?b)",
            "(pick-up ?a ?b ?c)"
            )

        val bonus = p.steps.map { step =>
          val prop = p.binding.substVarsShortString(step)
          //println("props = " + prop)
          if (preferred.exists(stepStringMatching(_, prop))) 20
          else 0
        }.foldLeft(0)((a, b) => a + b)
        
        val penalty = p.steps.map { step =>
          val prop = p.binding.substVarsShortString(step)
          //println("props = " + prop)
          if (disliked.exists(stepStringMatching(_, prop))) 20
          else 0
        }.foldLeft(0)((a, b) => a + b)
*/
        //println("**** bonus = " + bonus + "***")
        //val variables = p.steps flatMap {_.parameters} distinct
        //val unbounded = variables.filter{v => p.binding.getBoundedSymbol(v).isEmpty}.size

        past + future //- bonus + penalty //+ unbounded
      }
    }

  def stepStringMatching(string1: String, string2: String): Boolean = {
    val array1 = string1.split(" ")
    val array2 = string2.split(" ")
    if (array1.length != array2.length) return false
    for (i <- 0 until array1.length) {
      if ((!array1(i).startsWith("?")) && (!array2(i).startsWith("?")) && array1(i) != array2(i))
        return false
    }
    true
  }

  def evalOld(p: Plan): Double =
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
