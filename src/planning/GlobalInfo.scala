package planning
import variable._
import analogy._
import action._
import structures._

class Problem(
  val init: List[Proposition],
  val goal: List[Proposition],
  val subclasses: Map[String, Set[String]] = scala.collection.immutable.HashMap[String, Set[String]]()) {

  /** this method should only be used in TotalParser, where the Global Info object has not been constructed
   * Subject to refactorization
   */
  def isSubtype(subType: String, superType: String): Boolean = {
    if (subclasses.contains(superType))
      return subclasses(superType).contains(subType)
    else return false
  }
}

abstract class AbstractGlobal {

}

class GlobalInfo(var actionTemplates: List[Action], problem: Problem) {

  //var actionTemplates = List[Action]()
  var initState = problem.init
  var goalState = problem.goal
  var initialPlan: Plan = null
  var planVisited = 0
  var ontology: Map[String, Set[String]] = problem.subclasses

  protected var planGenerated = 0

  def newPlanID() =
    {
      planGenerated += 1
      planGenerated
    }

  /*
  def init() {
    actionTemplates = actions
    initState = problem.init
    goalState = problem.goal
    classes = problem.subclasses
  }
  */

  def isSubtype(subType: String, superType: String): Boolean = {
    if (ontology.contains(superType))
      return ontology(superType).contains(subType)
    else return false
  }

  def initPlan(): Plan = {
    val initStep = Action.initState(initState)
    val goalStep = Action(Constants.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalState, List[Proposition]())
    val flaws = goalStep.preconditions.map(x => new OpenCond(Constants.GOAL_ID, x))

    Plan(0,
      List[Action](initStep, goalStep),
      List[Link](),
      new Ordering(Set((0, Constants.GOAL_ID))),
      new Binding(),
      flaws,
      "initial plan",
      List[Record](),
      null,
      null)
  }
}

class DecompGlobal(actionTemplates: List[DecompAction], problem: Problem, val recipes: List[Recipe]) extends GlobalInfo(actionTemplates, problem) {

  override def initPlan(): Plan = {
    val initStep = Action(Constants.INIT_ID, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initState)
    val goalStep = Action(Constants.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalState, List[Proposition]())
    val flaws = goalStep.preconditions.map(x => new OpenCond(Constants.GOAL_ID, x))

    DecompPlan(0,
      List[Action](initStep, goalStep),
      List[Link](),
      List[DecompLink](),
      new Ordering(Set((0, Constants.GOAL_ID))),
      new Binding(),
      flaws,
      "initial plan",
      null,
      null)
  }
}

class GadgetGlobal(actionTemplates: List[Action], problem: Problem, protoPlan: Plan) extends GlobalInfo(actionTemplates, problem) {

  ontology = mergeClasses(ontology, problem.subclasses)
  println("all classes: " + ontology)
  val prototype = protoPlan
  var gadgetPlanning = true

  def mergeClasses(subclass1: Map[String, Set[String]], subclass2: Map[String, Set[String]]): Map[String, Set[String]] =
    {

      // handle situations involving nulls
      if (subclass1 == null) return subclass2
      else if (subclass2 == null) return subclass1

      var answer = subclass1
      // now the real thing
      subclass2.keys foreach {
        key =>
          val content2 = subclass2.get(key).get
          subclass1.get(key) match {
            case Some(content1) => answer += ((key, content1 ++ content2))
            case None => answer += ((key, content2))
          }
      }

      subclass1
    }
}

