package agagame
import java.io._
import variable.Proposition
import parsing._
import planning._
import bestfirst._
import structures._
import logging._
import action._
import scala.collection.mutable.ListBuffer

object GameMain {

  def main(args: Array[String]) {
    val (prob, actions) = try {
      TotalParser.parse("./planfiles/aga/aga.prob", "./planfiles/aga/aga.act")
    } catch {
      case e: RuntimeException =>
        println("cannot parse file: " + e.getMessage())
        e.printStackTrace()
        return
    }
    //DebugInfo.setDebug()
    var initState = prob.init

    val in = new BufferedReader(new InputStreamReader(System.in));
    var inputStr = ""

    while (true) {
      inputStr = in.readLine()
      if (inputStr != null) {
        val props = string2WorldState(inputStr)
        initState = updateInitState(initState, props)
        //println("initial state = " + initState)
      }
      
      plan(prob, actions, initState)
    }
  }

  def plan(problem: Problem, actions: List[Action], initState: List[Proposition]) = {

    val problems = generateProblems(problem, initState)
    val array = Array.ofDim[Float](5)
    var i = 0
    
    for (currentProb <- problems) {
      val g = new GlobalInfo(actions, currentProb)
      var plan = g.initPlan()
      println(plan.flaws)

      val parameter = new SearchParameter(50000)
      val search = new ExhaustiveSearch[Plan](List(plan), SimpleRepair.refine(g) _, Main.complete _, Main.eval _, parameter)

      val results = search.exhaustiveSearch(5000)
      array(i) = results.size
      i += 1
    }       
    
    array
  }

  def generateProblems(orginal: Problem, initState: List[Proposition]): List[Problem] =
    {

      val destructionGoal = List(Proposition.parse("(earth-destroyed)"))
      val fameGoal = List(Proposition.parse("(fame-restored villain:Person)"))
      val sidekickGoal = List(Proposition.parse("(not (alive villain:Person))"))

      val destructionProb = new Problem(initState, destructionGoal, orginal.ontology)
      val fameProb = new Problem(initState, fameGoal, orginal.ontology)
      val sidekickProb = new Problem(initState, sidekickGoal, orginal.ontology)

      List(destructionProb, fameProb, sidekickProb)
      //List(sidekickProb)
    }

  def updateInitState(oldState: List[Proposition], newState: List[Proposition]): List[Proposition] = {
    var state = ListBuffer[Proposition]() ++ oldState

    for (p <- newState) {
      if (p.verb == 'not) {
        state -= p.negate()
      } else if (!state.contains(p)) {
        state += p
      }
    }

    state.toList
  }

  def searchAllPossible() {

  }

  def testReadWorldState() {
    val in = new BufferedReader(new InputStreamReader(System.in));
    var str = ""
    while (true) {
      str = in.readLine()
      if (str != null) {
        val props = string2WorldState(str)
        println(props)
      }
    }
  }

  def string2WorldState(str: String): List[Proposition] = {

    var propositions = List[Proposition]()

    if (str != null) {
      if (str.length < 7) {
        println("badly formatted string: " + str)
      } else {
        for (i <- 0 to 6) {
          val num = str.substring(i, i + 1).toInt
          //println("num " + num)
          var pred = "("
          // The first three digits are about minions' lives
          if (i < 3) {
            if (num > 0)
              pred += " alive minion" + (i + 1) + ":Person )"
            else
              pred += "not (alive minion" + (i + 1) + ":Person ))"
          } else if (i < 6) {
            // from 4-6, children's health
            if (num > 0)
              pred += " sick kid" + (i - 2) + ":Person)"
            else
              pred += "not (sick kid" + (i - 2) + ":Person ))"
          } else if (i == 6) {
            // the final number is about if the sidekick has a sword
            if (num > 0)
              pred += " owns sidekick:Person sword:Object )"
            else
              pred += "not (owns sidekick:Person sword:Weapon) )"
          }

          propositions = Proposition.parse(pred) :: propositions
        }
      }
    }

    propositions
  }
}