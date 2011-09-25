package parsing

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

import variable._
import plan._

class PopParser extends JavaTokenParsers {
  def closing: Parser[Any] = """\s*\)\s*""".r
  //  {
  //    case (x:String, y:List[String])
  //  }
  def term: Parser[TopTerm] = popobject | variable | prop
  def string: Parser[String] = ("""[-\w]+""".r)
  def symbol: Parser[Symbol] = ("""[\w-\+]+""".r) ^^ { x => Symbol(x) }
  def variable: Parser[Variable] = """\?[-\+\w]+""".r ~ opt(":" ~ """[-\+\w]+""".r) ^^ {
    case x ~ Some(":" ~ y) => Variable(x, y)
    case x ~ None => Variable(x)
  }
  def popobject: Parser[PopObject] = ("""[\w-\+]+""".r) ~ opt(":" ~ """[-\+\w]+""".r) ^^ {
    case x ~ Some(":" ~ y) => PopObject(x, y)
    case x ~ None => PopObject(x)
  }
  def prop: Parser[Proposition] = "(" ~> symbol ~ rep(term) <~ ")" ^^ {
    case symbol ~ l => Proposition(symbol, l)
  }
}

object ActionParser extends PopParser {

  var inAction = false
  var typeHash = HashMap[String, Symbol]()

  def refreshHash() {
    typeHash = HashMap[String, Symbol]()
  }

  def readFile(file: String): List[Action] =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      // have to parse each action separately
      val ac = parseAll(actionlist, lines).get
      ac
    }

  def actionlist: Parser[List[Action]] = rep("""\s*""".r ~ action) ^^
    {
      case list =>
        list.map(x => x._2)
    }

  def action: Parser[Action] = "(action " ~> string ~ "(" ~ rep(variable) ~ closing ~
    "(" ~ "constraints " ~ rep(prop) ~ closing ~
    "(" ~ "preconditions" ~ rep(prop) ~ closing ~
    "(" ~ "effects" ~ rep(prop) <~ closing ~
    closing ^^
    {
      case string ~ "(" ~ list1 ~ x1 ~
        "(" ~ "constraints " ~ list2 ~ x2 ~
        "(" ~ "preconditions" ~ list3 ~ x3 ~
        "(" ~ "effects" ~ list4 =>
        {
          //          println("0: " + string + " 1: " + list1 + " 2: " + list2 + " 3: " + list3 + " 4 " + list4)
          Action(string, list1, list2, list3, list4)
        }
    }
}

object ProblemParser extends PopParser {
  def problem: Parser[Problem] = "(" ~ "problem" ~ "(" ~ "init" ~> rep(prop) ~
    ")" ~ "(" ~ "goal" ~ rep(prop) ~ ")" ~ opt("(" ~ "classes" ~ rep(prop) ~ ")") <~
    ")" ^^
    {
      case list1 ~
        ")" ~ "(" ~ "goal" ~ list2 ~ ")" ~ None => new Problem(list1, list2)
        
      case list1 ~
        ")" ~ "(" ~ "goal" ~ list2 ~ ")" ~ Some("(" ~ "classes" ~ list3 ~ ")") =>
          // this is where we read in subclass information
          // for now we do not infer subclasses
          // that is, all class hierarchies, explicit or implied, must be supplied by the user 
          var classHash = new HashMap[String, Set[String]]()
          list3 foreach {prop =>
            prop.verb match
            {
              case 'subclass =>
                val subclass = prop.termlist(0).asInstanceOf[PopObject].name
                val superclass = prop.termlist(1).asInstanceOf[PopObject].name
                if (classHash.contains(superclass))
                {
                  val set = classHash.get(superclass).get + subclass
                  classHash += (superclass -> set)
                }
                else
                  classHash += (superclass -> Set(subclass))
                  
              case _ => throw new PopParsingException("invalid propositions in the class section: " + prop)
            }
          }
          // convert it into an immutable hash map
          println(classHash.toMap[String, Set[String]])
          new Problem(list1, list2, classHash.toMap[String, Set[String]])
    }

  def readFile(file: String): Problem =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      val prob = parseAll(problem, lines).get
      prob
    }
}

object TotalParser {

  var typeHash = HashMap[String, String]()

  def parse(problemFile: String, actionFile: String): (Problem, List[Action]) =
    {
      typeHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      var listAction = ActionParser.readFile(actionFile)

      problem.init foreach { collectObjTypes(_) }
      problem.goal foreach { collectObjTypes(_) }

      val init = problem.init map { identifyTypes(_, typeHash) }
      val goal = problem.goal map { identifyTypes(_, typeHash) }

      problem = new Problem(init, goal, problem.subclasses)
      listAction = listAction map { findObjectTypesInActions(_) }

      (problem, listAction)
    }

  def findObjectTypesInActions(action: Action): Action =
    {
      if (action.parameters.exists(v => v.pType == "Any")) throw new PopParsingException("unspecified parameter type in action: " + action)

      var actionHash = typeHash.clone()
      action.parameters foreach { v =>
        if (actionHash.get(v.name).isDefined) throw new PopParsingException("name conflicts: " + v.name + " is repeated.")
        else
          actionHash += (v.name -> v.pType)
      }

      val newConstraints = action.constraints map { identifyTypes(_, actionHash) }

      val newPreconds = action.preconditions map { identifyTypes(_, actionHash) }
      val newEffects = action.effects map { identifyTypes(_, actionHash) }

      Action(action.name, action.parameters, newConstraints, newPreconds, newEffects)
    }

  /**
   * infers types for objects and variables from the hashmap
   *
   */
  def identifyTypes(p: Proposition, hashmap: HashMap[String, String]): Proposition =
    {
      val newtermlist = p.termlist map
        {
          _ match {
            case o: PopObject =>
              if (o.pType == "Any")
                hashmap.get(o.name) match {
                  case Some(storedType) => PopObject(o.name, storedType)
                  case None => throw new PopParsingException("The type of object " + o.name + " cannot be inferred.")
                }
              else o
            case p: Proposition => identifyTypes(p, hashmap)
            case v: Variable => if (v.pType == "Any")
              hashmap.get(v.name) match {
                case Some(storedType) => Variable(v.name, storedType)
                case None => throw new PopParsingException("The type of variable " + v.name + " cannot be inferred.")
              }
            else v
          }
        }
      new Proposition(p.verb, newtermlist)
    }

  /**
   * collects all specified object types from propositions in the problem specifications
   *
   */
  def collectObjTypes(prop: Proposition) {
    prop.termlist.foreach {
      _ match {
        case o: PopObject => if (o.pType != "Any") {
          typeHash.get(o.name) foreach
            { storedType =>
              if (storedType != o.pType) throw new PopParsingException("Conflicting types for object: " + o.name + " has type " + storedType + " and " + o.pType)
            }
          typeHash += (o.name -> o.pType)
        }
        case p: Proposition => collectObjTypes(p)
        case v: Variable => throw new PopParsingException("There should not be variables in problem specifications")
      }
    }
  }
}

class PopParsingException(val message: String) extends Exception(message)

object MainParser {
  def main(args: Array[String]) {
    val (prob, actions) = TotalParser.parse("./planfiles/test1.prob", "./planfiles/test1.act")
    println(prob.init)
    println(prob.goal)
    println(prob.subclasses)
    println
    actions foreach { a =>
      println(a)
      println(a.constraints)
      println(a.preconditions)
      println(a.effects)
      println
    }
  }
}