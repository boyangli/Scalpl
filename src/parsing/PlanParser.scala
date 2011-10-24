package parsing
import variable._
import plan._
import scala.collection.mutable.HashMap

object PlanParser extends AbstractPlanParser {

  protected var actionTemplates = List[Action]()
  protected var planBinding = new Binding()

  //  override def variable: Parser[Variable] = """\?[\+\w]+""".r ~ "-" ~ integer ^^ {
  //    case x ~ "-" ~ y => new Variable(x, "Any", y.toInt)
  //  }

  protected override def variable: Parser[Variable] = """\?[\+\w-]+-[0-9]+""".r ^^ {
    x =>
      val index = x.lastIndexOf("-")
      val name = x.substring(0, index)
      val number = x.substring(index + 1).toInt
      Variable(name, "Any", number)
  }

  protected def fullobject: Parser[PopObject] = ("""[\w-\+]+""".r) ~ ":" ~ """[-\+\w]+""".r ^^ {
    case x ~ ":" ~ y => PopObject(x, y)
  }

  //def token: Parser[Token] = popobject | variable

  protected override def term: Parser[TopTerm] = prop | token

  override def prop: Parser[Proposition] = symbol ~ "(" ~ rep(term) <~ ")" ^^ {
    case symbol ~ "(" ~ list => Proposition(symbol, list)
  }

  protected def objects: Parser[List[PopObject]] = "(" ~ "objects" ~> rep(fullobject) <~ ")"

  protected def integer: Parser[Int] = """[0-9]+""".r ^^ { x => x.toInt }

  protected def goalId: Parser[Int] = "goal" ^^ { x => Global.GOAL_ID }

  protected def stepId: Parser[Int] = integer | goalId

  protected def step: Parser[Action] = "[" ~> integer ~ "]" ~ "(" ~ rep(string) <~ ")" ^^ {
    case number ~ "]" ~ "(" ~ terms =>
      val temp = actionTemplates.find(action => action.name == terms(0))

      if (temp.isEmpty)
        throw new PopParsingException("Cannot find corresponding action " + terms.mkString(" "))

      val template = temp.get.instantiate(number)

      planBinding = planBinding.addNeqs(template.constraints.filter(_.verb == 'neq))

      val realParas = terms.tail map (objectHash.get(_).get)

      planBinding.unify(new Proposition('temp, template.parameters), new Proposition('temp, realParas), List[Proposition](), List[Proposition]()) match {
        case Some(b) => planBinding = b
        case None =>
          {
            //println(template.parameters.mkString(" "))
            //println(realParas.mkString(" "))
            throw new PopParsingException("Cannot bind corresponding action parameters: " + terms.mkString(" "))
          }
      }

      template
  }

  protected def link: Parser[Link] = "(" ~> stepId ~ "->" ~ stepId ~ ":" ~ prop ~ prop <~ ")" ^^ {
    case id1 ~ "->" ~ id2 ~ ":" ~ prop1 ~ prop2 =>
      new Link(id1, id2, prop1, prop2)
  }

  protected def stepPair: Parser[(Int, Int)] = "(" ~> stepId ~ "," ~ stepId <~ ")" ^^ { case id1 ~ "," ~ id2 => (id1, id2) }

  protected def ordering: Parser[Ordering] = "(" ~ "orderings" ~> rep(stepPair) <~ ")" ^^ { ods => new Ordering(ods.toSet[(Int, Int)]) }

  protected def planParse: Parser[Plan] = "(" ~ "initial-state" ~> rep(prop) ~ ")" ~ "(" ~ "goal-state" ~ rep(prop) ~ ")" ~ "(" ~ "steps" ~
    rep(step) ~ ")" ~ "(" ~ "links" ~ rep(link) ~ ")" ~ ordering ^^ {

      case initList ~ ")" ~ "(" ~ "goal-state" ~ goalsList ~ ")" ~ "(" ~ "steps" ~ stepList ~ ")" ~ "(" ~ "links" ~ linkList ~ ")" ~ newOrder =>
        // construct a initial step and a goal step
        val initStep = Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initList.map(substObj))
        val goalStep = Action(Global.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalsList.map(substObj), List[Proposition]())

        val newSteps = initStep :: goalStep :: stepList

        Plan.getEmpty().copy(id = 0,
          steps = newSteps,
          links = linkList,
          ordering = newOrder,
          stepCount = stepList.length)
    }

  protected def frameParse: Parser[ProtoFrame] = "(" ~ "name" ~> string ~ ")" ~ "(" ~ "initial-state" ~ rep(prop) ~ ")" ~ "(" ~ "goal-state" ~ rep(prop) ~ ")" ~ "(" ~ "steps" ~
    rep(step) ~ ")" ~ "(" ~ "links" ~ rep(link) ~ ")" ~ ordering ^^ {

      case name ~ ")" ~ "(" ~ "initial-state" ~ initList ~ ")" ~ "(" ~ "goal-state" ~
        goalsList ~ ")" ~ "(" ~ "steps" ~ stepList ~ ")" ~ "(" ~ "links" ~ linkList ~ ")" ~
        newOrder =>
        // construct a initial step and a goal step
        val initStep = Action(0, "init-state", List[Variable](), List[Proposition](), List[Proposition](), initList.map(substObj))
        val goalStep = Action(Global.GOAL_ID, "goal", List[Variable](), List[Proposition](), goalsList.map(substObj), List[Proposition]())

        val newSteps = initStep :: goalStep :: stepList

        new ProtoFrame(name,
          steps = newSteps,
          links = linkList,
          ordering = newOrder,
          new Binding())
    }

  protected def substObj(termlist: List[TopTerm]): List[TopTerm] =
    {
      termlist map {
        _ match {
          case obj: PopObject => if (obj.pType == "Any") objectHash.get(obj.name).get else obj
          case prop: Proposition => new Proposition(prop.verb, substObj(prop.termlist))
          case x: Variable => x
        }
      }
    }

  protected def substObj(p: Proposition): Proposition =
    {
      val newlist = substObj(p.termlist)

      new Proposition(p.verb, newlist)
    }

  def parseFrame(actionFile: String, problemFile: String, planFile: String): (Problem, List[Action], ProtoFrame) =
    {
      actionTemplates = ActionParser.readFile(actionFile)
      // we need the problem file to read in the class hierarchies
      val problem = ProblemParser.readFile(problemFile)
      // should we set subclasses when reading a frame?
      Global.classes = problem.subclasses

      val plantext = scala.io.Source.fromFile(planFile)

      val objlines = plantext.getLine(0)

      val objs = parseAll(objects, objlines).get

      objectHash.clear
      objectHash ++= objs.map { obj => (obj.name, obj) }

      actionTemplates = actionTemplates map { appendTypesToTemplate }

      //println(objectHash)

      val steplines = plantext.mkString

      var frame: ProtoFrame = parseAll(frameParse, steplines).get

      objectHash ++=
        frame.steps flatMap {
          _.parameters map {
            para => (para.fullName, para)
          }
        }

      val planlinks = frame.links map {
        link => new Link(link.id1, link.id2, appendTypesTo(link.effect), appendTypesTo(link.precondition))
      }

      frame = frame.copy(
        links = planlinks,
        binding = planBinding)

      //println(plan.links.map(_.toFileString()).mkString("\n"))
      //println(frame.links(2).effect)
      //println(frame.links(2).precondition)
      //println(frame.detailString())
      (problem, actionTemplates, frame)
    }

  def parse(actionFile: String, problemFile: String, planFile: String): (List[Action], Plan) =
    {
      actionTemplates = ActionParser.readFile(actionFile)
      // we need the problem file to read in the class hierarchies
      val problem = ProblemParser.readFile(problemFile)
      Global.classes = problem.subclasses

      val plantext = scala.io.Source.fromFile(planFile)

      val objlines = plantext.getLine(0)

      val objs = parseAll(objects, objlines).get

      objectHash.clear
      objectHash ++= objs.map { obj => (obj.name, obj) }

      actionTemplates = actionTemplates map { appendTypesToTemplate }

      println(objectHash)

      val steplines = plantext.mkString

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
        binding = planBinding)

      //println(plan.links.map(_.toFileString()).mkString("\n"))
//      println(plan.links(2).effect)
//      println(plan.links(2).precondition)
//      println(plan.detailString())
      (actionTemplates, plan)
    }

  def main(args: Array[String]) {
    //    val lines = "(objects jack:Person tom:Person school:Location money:Object home:Location)"
    //    val objs = parseAll(objects, lines).get
    //    println(objs)

//    val lines = """?p-e1-1"""
//
//    def vari: Parser[Variable] = """\?[\+\w-]+-[0-9]+""".r ^^ {
//      x =>
//        val index = x.lastIndexOf("-")
//        val name = x.substring(0, index)
//        val number = x.substring(index + 1).toInt
//        Variable(name, "Any", number)
//    }


    val frame = parseFrame("./planfiles/toyphone.act", "./planfiles/toyphone.prob", "./planfiles/toyphone.plan")
    println(frame._3.planString)
  }
}