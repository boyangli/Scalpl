package parsing
import variable._
import plan._
import scala.collection.mutable.HashMap

object PlanParser extends PopParser {

  var actionTemplates = List[Action]()
  var planBinding = new Binding()
  var objectHash = HashMap[String, Token]()

  override def variable: Parser[Variable] = "v-?" ~> """[-\+\w]+""".r ~ "-" ~ """[0-9]+""".r ^^ {
    case x ~ "-" ~ y => new Variable(x, "Any", y.toInt)
  }

  def fullobject: Parser[PopObject] = ("""[\w-\+]+""".r) ~ ":" ~ """[-\+\w]+""".r ^^ {
    case x ~ ":" ~ y => PopObject(x, y)
  }
  
  override def prop: Parser[Proposition] = symbol ~ "(" ~ rep(term) <~ ")" ^^ {
    case symbol ~ "(" ~ list => Proposition(symbol, list)
  }

  def token: Parser[Token] = popobject | variable

  def objects: Parser[List[PopObject]] = "(" ~ "objects" ~> rep(fullobject) <~ ")"

  def step: Parser[Action] = "[" ~> """[0-9]+""".r ~ "]" ~ "(" ~ rep(string) <~ ")" ^^ {
    case number ~ "]" ~ "(" ~ terms =>
      val temp = actionTemplates.find(action => action.name == terms(0))

      if (temp.isEmpty) 
        throw new PopParsingException("Cannot find corresponding action " + terms.mkString(" "))

      val template = temp.get.instantiate(number.toInt)

      planBinding = planBinding.addNeqs(template.constraints.filter(_.verb == 'neq))

      val realParas = terms.tail map (objectHash.get(_).get)

      planBinding.unify(new Proposition('temp, template.parameters), new Proposition('temp, realParas), List[Proposition](), List[Proposition]()) match {
        case Some(b) => planBinding = b
        case None => throw new PopParsingException("Cannot binding corresponding action parameters: " + terms.mkString(" "))
      }

      template
      
  }

  def planParse: Parser[Plan] = "(" ~ "initial-state" ~> rep(prop) ~ ")" ~ "(" ~ "goal-state" ~ rep(prop) ~ ")" ~ "(" ~ "steps" ~ rep(step) <~ ")" ^^ {
    case initList ~ ")" ~ "(" ~ "goal-state" ~ goalsList ~ ")" ~ "(" ~ "steps" ~ stepList =>
      // construct a initial step and a goal step
      val initStep = new Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initList.map(substObj))
      val goalStep = new Action(Global.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalsList.map(substObj), List[Proposition]())

      val newSteps = initStep :: goalStep :: stepList

      Plan.getEmpty().copy(id = 0,
        steps = newSteps,
        stepCount = stepList.length)
  }

  def substObj(termlist: List[TopTerm]): List[TopTerm] =
    {
      termlist map {
        _ match {
          case obj: PopObject => if (obj.pType == "Any") objectHash.get(obj.name).get else obj
          case prop: Proposition => new Proposition(prop.verb, substObj(prop.termlist))
          case x: Variable => x
        }
      }
    }

  def substObj(p: Proposition): Proposition =
    {
      val newlist = substObj(p.termlist)

      new Proposition(p.verb, newlist)
    }

  //  def planOverall:Parser[(List[PopObject], Plan)] = objects ~ planParse ^^ {
  //    case x ~ y => (x, y)
  //  }

  def parse(actionFile: String, planFile: String): (List[Action], Plan) =
    {
      actionTemplates = ActionParser.readFile(actionFile)

      val objlines = "(objects jack:Person tom:Person school:Location money:Object home:Location)"

      val objs = parseAll(objects, objlines).get

      objectHash.clear
      objectHash ++= objs.map { obj => (obj.name, obj) }

      println(objectHash)

      val steplines = """(initial-state owns(jack money) at(tom school) at(jack home))
      (goal-state owns(tom money) dead(jack) at(tom home))
      (steps 
      [2] (move tom school home)
      [4] (kill tom jack home)
      [3] (dead-drop jack money home)
      [1] (pick-up tom money home)
      )"""

      val as1 = parseAll(planParse, steplines).get
      println(as1)
      println(as1.steps.map(planBinding.substVarsShortString))
      //println(planBinding)

      var planSteps = List[Action]()
      var planLinks = List[Link]()
      var planOrdering = new Ordering()
      

      val plan = Plan.getEmpty().copy(
        id = 0,
        steps = planSteps,
        links = planLinks,
        ordering = planOrdering,
        binding = planBinding)

      (actionTemplates, plan)
    }

  def main(args: Array[String]) {
    //    val lines = "(objects jack:Person tom:Person school:Location money:Object home:Location)"
    //    val objs = parseAll(objects, lines).get
    //    println(objs)

//    val lines = """[2] (move tom school home)"""
//    //def init: Parser[List[Proposition]] = "(" ~ "initial-state" ~> rep(prop) <~ ")"
//    def init: Parser[List[Action]] = rep(step)
//    val objs = parseAll(step, lines).get
//    println(objs)

    parse("./planfiles/test2.act", "./planfiles/plantext.txt")
  }
}