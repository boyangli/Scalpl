package parsing

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

import variable._
import plan._

object TotalParser extends AbstractPlanParser {

  def parse(problemFile: String, actionFile: String): (Problem, List[Action]) =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      var listAction = ActionParser.readFile(actionFile)

      //      problem.init foreach { collectObjTypes(_) }
      //      problem.goal foreach { collectObjTypes(_) }

      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }

      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }

      problem = new Problem(init, goal, problem.subclasses)
      listAction = listAction map { appendTypesToTemplate(_) }

      (problem, listAction)
    }

  def parseProblem(problemFile: String): Problem =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }
      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }
      new Problem(init, goal, problem.subclasses)
    }
  

  /**
   * collects all specified object types from propositions in the problem specifications
   *
   */
  //  def collectObjTypes(prop: Proposition) {
  //    prop.termlist.foreach {
  //      _ match {
  //        case o: PopObject => if (o.pType != "Any") {
  //          typeHash.get(o.name) foreach
  //            { storedType =>
  //              if (storedType != o.pType) throw new PopParsingException("Conflicting types for object: " + o.name + " has type " + storedType + " and " + o.pType)
  //            }
  //          typeHash += (o.name -> o.pType)
  //        }
  //        case p: Proposition => collectObjTypes(p)
  //        case v: Variable => throw new PopParsingException("There should not be variables in problem specifications")
  //      }
  //    }
  //  }
}