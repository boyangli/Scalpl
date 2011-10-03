package parsing
import variable._
import plan._
import scala.collection.mutable.HashMap

object PlanParser extends AbstractPlanParser {

  var actionTemplates = List[Action]()
  var planBinding = new Binding()

  //  override def variable: Parser[Variable] = """\?[\+\w]+""".r ~ "-" ~ integer ^^ {
  //    case x ~ "-" ~ y => new Variable(x, "Any", y.toInt)
  //  }

  override def variable: Parser[Variable] = """\?[\+\w-]+-[0-9]+""".r ^^ {
    x =>
      val index = x.lastIndexOf("-")
      val name = x.substring(0, index)
      val number = x.substring(index + 1).toInt
      Variable(name, "Any", number)
  }

  def fullobject: Parser[PopObject] = ("""[\w-\+]+""".r) ~ ":" ~ """[-\+\w]+""".r ^^ {
    case x ~ ":" ~ y => PopObject(x, y)
  }

  def token: Parser[Token] = popobject | variable

  override def term: Parser[TopTerm] = prop | token

  override def prop: Parser[Proposition] = symbol ~ "(" ~ rep(term) <~ ")" ^^ {
    case symbol ~ "(" ~ list => Proposition(symbol, list)
  }

  def objects: Parser[List[PopObject]] = "(" ~ "objects" ~> rep(fullobject) <~ ")"

  def integer: Parser[Int] = """[0-9]+""".r ^^ { x => x.toInt }

  def goalId: Parser[Int] = "goal" ^^ { x => Global.GOAL_ID }

  def stepId: Parser[Int] = integer | goalId

  def step: Parser[Action] = "[" ~> integer ~ "]" ~ "(" ~ rep(string) <~ ")" ^^ {
    case number ~ "]" ~ "(" ~ terms =>
      val temp = actionTemplates.find(action => action.name == terms(0))

      if (temp.isEmpty)
        throw new PopParsingException("Cannot find corresponding action " + terms.mkString(" "))

      val template = temp.get.instantiate(number)

      planBinding = planBinding.addNeqs(template.constraints.filter(_.verb == 'neq))

      val realParas = terms.tail map (objectHash.get(_).get)

      planBinding.unify(new Proposition('temp, template.parameters), new Proposition('temp, realParas), List[Proposition](), List[Proposition]()) match {
        case Some(b) => planBinding = b
        case None => throw new PopParsingException("Cannot binding corresponding action parameters: " + terms.mkString(" "))
      }

      template
  }

  def link: Parser[Link] = "(" ~> stepId ~ "->" ~ stepId ~ ":" ~ prop ~ prop <~ ")" ^^ {
    case id1 ~ "->" ~ id2 ~ ":" ~ prop1 ~ prop2 =>
      new Link(id1, id2, prop1, prop2)
  }

  def stepPair: Parser[(Int, Int)] = "(" ~> stepId ~ "," ~ stepId <~ ")" ^^ { case id1 ~ "," ~ id2 => (id1, id2) }

  def ordering: Parser[Ordering] = "(" ~ "orderings" ~> rep(stepPair) <~ ")" ^^ { ods => new Ordering(ods.toSet[(Int, Int)]) }

  def planParse: Parser[Plan] = "(" ~ "initial-state" ~> rep(prop) ~ ")" ~ "(" ~ "goal-state" ~ rep(prop) ~ ")" ~ "(" ~ "steps" ~
    rep(step) ~ ")" ~ "(" ~ "links" ~ rep(link) ~ ")" ~ ordering ^^ {

      case initList ~ ")" ~ "(" ~ "goal-state" ~ goalsList ~ ")" ~ "(" ~ "steps" ~ stepList ~ ")" ~ "(" ~ "links" ~ linkList ~ ")" ~ newOrder =>
        // construct a initial step and a goal step
        val initStep = new Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initList.map(substObj))
        val goalStep = new Action(Global.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalsList.map(substObj), List[Proposition]())

        val newSteps = initStep :: goalStep :: stepList

        Plan.getEmpty().copy(id = 0,
          steps = newSteps,
          links = linkList,
          ordering = newOrder,
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
      
      val plantext = scala.io.Source.fromFile(planFile)

      val objlines = plantext.getLine(0)

      val objs = parseAll(objects, objlines).get

      objectHash.clear
      objectHash ++= objs.map { obj => (obj.name, obj) }

      actionTemplates = actionTemplates map { appendTypesToTemplate }

      //println(objectHash)

      val steplines = plantext.mkString 

      /*val steplines = """(initial-state owns(jack money) at(tom school) at(jack home))
      (goal-state owns(tom money) dead(jack) at(tom home))
      (steps 
      [2] (move tom school home)
      [4] (kill tom jack home)
      [3] (dead-drop jack money home)
      [1] (pick-up tom money home)
      )
      (links (2 -> goal: at(?p1-2 ?loc2-2) at(tom home))
(4 -> goal: dead(?p2-4) dead(jack))
(2 -> 1: at(?p1-2 ?loc2-2) at(?p1-1 ?loc-1))
(0 -> 4: at(jack home) at(?p2-4 ?loc-4))
(2 -> 4: at(?p1-2 ?loc2-2) at(?p1-4 ?loc-4))
(0 -> 4: closedworld() not(dead(?p2-4)))
(0 -> 4: closedworld() not(dead(?p1-4)))
(4 -> 3: dead(?p2-4) dead(?p1-3))
(0 -> 3: owns(jack money) owns(?p1-3 ?o-3))
(0 -> 3: at(jack home) at(?p1-3 ?loc-3))
(3 -> 1: for-pickup(?o-3) for-pickup(?o-1))
(0 -> 2: closedworld() not(dead(?p1-2)))
(0 -> 2: at(tom school) at(?p1-2 ?loc1-2))
(2 -> 1: at(?p1-2 ?loc2-2) at(?p1-1 ?loc-1))
(1 -> goal: owns(?p1-1 ?o-1) owns(tom money)))
      (orderings (0,3) (4,3) (2,goal) (0,4) (3,goal) (4,goal) (2,4) (2,1) (0,1) (1,goal) (0,goal) (3,1) (0,2))
      """
      */

      var plan: Plan = parseAll(planParse, steplines).get

      objectHash ++=
        plan.steps flatMap {
          _.parameters map {
            para => (para.fullName, para)
          }
        }

      val planlinks = plan.links map {
        link => new Link(link.id1, link.id2, appendTypesTo(link.effect), appendTypesTo(link.precondition))
      }

      plan = plan.copy(
        links = planlinks,
        binding = planBinding
        )

      //println(plan.links.map(_.toFileString()).mkString("\n"))
        println(plan.links(2).effect)
        println(plan.links(2).precondition)
        println(plan.detailString())
      (actionTemplates, plan)
    }

  def main(args: Array[String]) {
    //    val lines = "(objects jack:Person tom:Person school:Location money:Object home:Location)"
    //    val objs = parseAll(objects, lines).get
    //    println(objs)

    val lines = """?p-e1-1"""

    def vari: Parser[Variable] = """\?[\+\w-]+-[0-9]+""".r ^^ {
      x =>
        val index = x.lastIndexOf("-")
        val name = x.substring(0, index)
        val number = x.substring(index + 1).toInt
        Variable(name, "Any", number)
    }
    //    def init: Parser[Any] = rep(link)
    //    val objs = parseAll(vari, lines).get
    //    def init: Parser[List[Link]] = rep(link)
    //    val objs = parseAll(init, lines).get
    //    println(objs)
    //    println(objs.name)
    //    println(objs.pType)
    //    println(objs.number)
    //println(objs.isInstanceOf[TopTerm])

    parse("./planfiles/test2.act", "./planfiles/plantext.txt")
  }
}