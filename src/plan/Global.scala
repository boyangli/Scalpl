package plan
import variable._

class Problem(val init: List[Proposition], val goal: List[Proposition])

object Global {

  val GOAL_ID = 65535

  var actionTemplates = List[Action]()
  var initState = List[Proposition]()
  var goalState = List[Proposition]()
  var initialPlan: Plan = null
  var planVisited = 0
  protected var planGenerated = 0
  var debug = false

  def obtainID() =
    {
      planGenerated += 1
      planGenerated
    }

  def init(actions: List[Action], problem: Problem) {
    actionTemplates = actions
    initState = problem.init
    goalState = problem.goal
  }

  def initPlan(): Plan = {
    val initStep = new Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initState)
    val goalStep = new Action(GOAL_ID, "goal", List[Variable](), List[Proposition](), goalState, List[Proposition]())
    val flaws = goalStep.preconditions.map(x => new OpenCond(GOAL_ID, x))
    
    Plan(0,
      List[Action](initStep, goalStep),
      List[Link](),
      new Ordering(List((0, GOAL_ID))),
      new Binding(),
      flaws,
      "initial plan",
      null,
      null)
  }
}