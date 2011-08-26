package parsing

import scala.util.parsing.combinator._
import variable._
import plan._

class PopParser extends JavaTokenParsers {
  def closing: Parser[Any] = """\s*\)\s*""".r
  //  {
  //    case (x:String, y:List[String])
  //  }
  def term: Parser[TopTerm] = symbol | variable | prop
  def string: Parser[String] = ("""[\w-]+""".r)
  def symbol: Parser[PopSymbol] = ("""[\w-\+]+""".r) ^^ { x => PopSymbol(Symbol(x)) }
  def variable: Parser[Variable] = """\?[\w-\+]+""".r ^^ { x => Variable(x) }
  def prop: Parser[Proposition] = "(" ~> symbol ~ rep(term) <~ ")" ^^
    {
      case symbol ~ l => Proposition(symbol, TermList(l))
    }
}

object ActionParser extends PopParser {

//  def parseProp(string:String):Proposition = parseAll(prop, string).get
  
  def readFile(file: String): List[Action] =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
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
    ")" ~ "(" ~ "goal" ~ rep(prop) <~ ")" ~ ")" ^^
    {
      case list1 ~
        ")" ~ "(" ~ "goal" ~ list2 => new Problem(list1, list2)
    }

  def readFile(file: String): Problem =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      val prob = parseAll(problem, lines).get
      prob
    }
}



object MainParser {
  def main(args: Array[String]) {
    val al = ActionParser.readFile("./planfiles/actions.txt")
    println(al.mkString)

    val problem = ProblemParser.readFile("./planfiles/problem.txt")
    println(problem.init + "\n" + problem.goal)
  }
}