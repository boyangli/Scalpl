package plan
import variable._

class Problem(
  val init: List[Proposition],
  val goal: List[Proposition],
  val subclasses: Map[String, Set[String]] = scala.collection.immutable.HashMap[String, Set[String]]())

object Global {

  val GOAL_ID = 65535

  var actionTemplates = List[Action]()
  var initState = List[Proposition]()
  var goalState = List[Proposition]()
  var initialPlan: Plan = null
  var planVisited = 0
  var classes: Map[String, Set[String]] = null
  var prototype: Plan = null
  var gadgetPlanning = false
  protected var planGenerated = 0
  protected var debugLvl = 'normal
  val debugFile = "log.txt"

  def newPlanID() =
    {
      planGenerated += 1
      planGenerated
    }

  def init(actions: List[Action], problem: Problem) {
    actionTemplates = actions
    initState = problem.init
    goalState = problem.goal
    classes = problem.subclasses
  }

  def init(actions: List[Action], problem: Problem, protoPlan: Plan) {
    actionTemplates = actions
    initState = problem.init
    goalState = problem.goal
    classes = mergeClasses(classes, problem.subclasses)
    println("all classes: " + classes)
    prototype = protoPlan
    gadgetPlanning = true
  }
  
  def mergeClasses(subclass1:Map[String, Set[String]], subclass2:Map[String, Set[String]]): Map[String, Set[String]] =
  {
    
    // handle situations involving nulls
    if (subclass1 == null) return subclass2
    else if (subclass2 == null) return subclass1
    
    var answer = subclass1
    // now the real thing
    subclass2.keys foreach {
      key => 
        val content2 = subclass2.get(key).get
        subclass1.get(key) match
        {
          case Some(content1) => answer += ((key, content1 ++ content2))
          case None => answer += ((key, content2))
        }
    }
    
    subclass1
  }

  def initPlan(): Plan = {
    val initStep = Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initState)
    val goalStep = Action(GOAL_ID, "goal", List[Variable](), List[Proposition](), goalState, List[Proposition]())
    val flaws = goalStep.preconditions.map(x => new OpenCond(GOAL_ID, x))

    Plan(0,
      List[Action](initStep, goalStep),
      List[Link](),
      new Ordering(Set((0, GOAL_ID))),
      new Binding(),
      flaws,
      "initial plan",
      List[Record](),
      List[Matching](),
      null,
      null)
  }

  def setNormal() {
    debugLvl = 'normal
  }

  def setTrace() {
    debugLvl = 'trace
  }

  def setDebug() {
    debugLvl = 'debug
  }

  def debug() = debugLvl
}