package planning
import variable._
import analogy._
import action._
import structures._
import jimpl._

class Problem(
  val init: List[Proposition],
  val goal: List[Proposition],
  val ontology:Ontology) {

}

abstract class AbstractGlobal {

}

class GlobalInfo(var actionTemplates: List[Action], problem: Problem) {

  //var actionTemplates = List[Action]()
  var initState = problem.init
  var goalState = problem.goal
  var initialPlan: Plan = null
  var planVisited = 0
  var ontology:Ontology = problem.ontology

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

  def initPlan(): Plan = {
    val initStep = Action.initState(initState)
    val goalStep = Action(Constants.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalState, List[Proposition]())
    val flaws = goalStep.preconditions.map(x => new OpenCond(Constants.GOAL_ID, x))

    Plan(0,
      List[Action](initStep, goalStep),
      List[Link](),
      new OrderingFaster(),
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
      new OrderingFaster(),
      new Binding(),
      flaws,
      "initial plan",
      null,
      null)
  }
}

class GadgetGlobal(actionTemplates: List[Action], problem: Problem, protoPlan: Plan) extends GlobalInfo(actionTemplates, problem) {

  /*
  ontology = ontology.mergeWith(problem.ontology)
  println("all classes: " + ontology)
  val prototype = protoPlan
  var gadgetPlanning = true
	*/

}

