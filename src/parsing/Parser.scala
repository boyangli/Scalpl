package parsing

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap

import variable._
import planning._
import structures._
import action._

class PopParser extends JavaTokenParsers {
  def closing: Parser[Any] = """\s*\)\s*""".r
  //  {
  //    case (x:String, y:List[String])
  //  }
  protected def term: Parser[TopTerm] = popobject | variable | prop
  protected def token: Parser[Token] = popobject | variable
  protected def string: Parser[String] = ("""[-\w]+""".r)
  protected def symbol: Parser[Symbol] = ("""[\w-\+]+""".r) ^^ { x => Symbol(x) }
  protected def variable: Parser[Variable] = """\?[-\+\w]+""".r ~ opt(":" ~ """[-\+\w]+""".r) ^^ {
    case x ~ Some(":" ~ y) => Variable(x, y)
    case x ~ None => Variable(x)
  }
  protected def popobject: Parser[PopObject] = ("""[\w-\+]+""".r) ~ opt(":" ~ """[-\+\w]+""".r) ^^ {
    case x ~ Some(":" ~ y) => PopObject(x, y)
    case x ~ None => PopObject(x)
  }

  protected def integer: Parser[Int] = """[0-9]+""".r ^^ { x => x.toInt }
  def prop: Parser[Proposition] = "(" ~> symbol ~ rep(term) <~ ")" ^^ {
    case symbol ~ l => Proposition(symbol, l)
  }
}

object PopParser {
  //def parseProposition(string: String): Proposition = parseAll(prop, string).get
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
        list3 foreach { prop =>
          prop.verb match {
            case 'subclass =>
              val subclass = prop.termlist(0).asInstanceOf[PopObject].name
              val superclass = prop.termlist(1).asInstanceOf[PopObject].name
              if (classHash.contains(superclass)) {
                val set = classHash.get(superclass).get + subclass
                classHash += (superclass -> set)
              } else
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
      val result = parseAll(problem, lines)
      result match {
        case Success(x, _) => return x
        case NoSuccess(err, next) => {
          println("failed to parse input as planning problem" +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          throw new PopParsingException("planning problem parsing failure.")
        }
      }
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