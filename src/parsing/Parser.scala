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
  def token: Parser[Token] = popobject | variable
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
  
  def actorParser: Parser[Token] = "(" ~ "actor" ~> token <~ closing 

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
  opt(actorParser) ~
    "(" ~ "constraints " ~ rep(prop) ~ closing ~
    "(" ~ "preconditions" ~ rep(prop) ~ closing ~
    "(" ~ "effects" ~ rep(prop) <~ closing ~
    closing ^^
    {
      case name ~ "(" ~ list1 ~ x1 ~ actor ~
        "(" ~ "constraints " ~ list2 ~ x2 ~
        "(" ~ "preconditions" ~ list3 ~ x3 ~
        "(" ~ "effects" ~ list4 =>
        {
          //          println("0: " + string + " 1: " + list1 + " 2: " + list2 + " 3: " + list3 + " 4 " + list4)
          actor match
          {
            case Some(act) => Action(name, act, list1, list2, list3, list4)
            case None => Action(name, list1, list2, list3, list4)
          }
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



class PopParsingException(val message: String) extends Exception(message)

object MainParser {
  def main(args: Array[String]) {
    val (prob, actions) = TotalParser.parse("./planfiles/test1.prob", "./planfiles/toyphone.act")
    println(prob.init)
    println(prob.goal)
    println(prob.subclasses)
    println
    actions foreach { a =>
      println(a)
      println(a.actor)
      println(a.constraints)
      println(a.preconditions)
      println(a.effects)
      println
    }
  }
}