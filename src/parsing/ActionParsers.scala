package parsing

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import java.io._
import planning._
import structures._
import action._
import variable._

abstract class AbstractActionParser extends PopParser {
  def readFile(file: String): List[Action]

  def actorParser: Parser[Token] = "(" ~ "actor" ~> token <~ closing
}

object ActionParser extends AbstractActionParser {

  def parseProposition(string: String): Proposition = parseAll(prop, string).get

  //var inAction = false
  //var typeHash = HashMap[String, Symbol]()

  //  def refreshHash() {
  //    typeHash = HashMap[String, Symbol]()
  //  }

  def readFile(file: String): List[Action] =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      // have to parse each action separately
      val result = parseAll(actionlist, lines)

      result match {
        case Success(x, _) => {
          x foreach { _.testValid }
          return x
        }
        case NoSuccess(err, next) => {
          println("failed to parse input as actions" +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          throw new PopParsingException("action parsing failure.")
        }
      }
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
          actor match {
            case Some(act) => Action(name, act, list1, list2, list3, list4)
            case None => Action(name, list1, list2, list3, list4)
          }
        }
    }
}

object DecompActionParser extends AbstractActionParser {

  def readFile(file: String): List[DecompAction] =
    {
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      // have to parse each action separately
      val result = parseAll(actionlist, lines)

      result match {
        case Success(x, _) => {
          x foreach { _.testValid() }
          return x
        }
        case NoSuccess(err, next) => {
          println("failed to parse input as actions" +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          throw new PopParsingException("action parsing failure.")
        }
      }
    }

  def actionlist: Parser[List[DecompAction]] = rep("""\s*""".r ~ action) ^^
    {
      case list =>
        list.map(x => x._2)
    }

  def action: Parser[DecompAction] = simpleAction | compositeAction

  def simpleAction: Parser[DecompAction] = "(action " ~> string ~ "(" ~ rep(variable) ~ closing ~
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
          actor match {
            case Some(act) => DecompAction(name, act, list1, list2, list3, list4, false)
            case None => DecompAction(name, list1, list2, list3, list4, false)
          }
        }
    }

  def compositeAction: Parser[DecompAction] = "(composite action " ~> string ~ "(" ~ rep(variable) ~ closing ~
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
        //          println("0: " + string + " 1: " + list1 + " 2: " + list2 + " 3: " + list3 + " 4 " + list4)

        actor match {
          case Some(act) => DecompAction(name, act, list1, list2, list3, list4, true)
          case None => DecompAction(name, list1, list2, list3, list4, true)
        }
    }
}