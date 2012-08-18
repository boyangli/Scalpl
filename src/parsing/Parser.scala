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
      case init ~
        ")" ~ "(" ~ "goal" ~ goal ~ ")" ~ None =>
          // the subclass list is not supplied
        var objectHash = HashMap[String, PopObject]()
        init.foreach(collectTypes(_, objectHash))
        goal.foreach(collectTypes(_, objectHash))
        val subclasses = Map[String, Set[String]]() ++ objectHash.values.map(_.pType).toList.distinct.map((_, Set[String]()))
        
        val ontology = new Ontology(subclasses, objectHash.toMap)
        val newInit = init map { ontology.appendTypesTo }
        val newGoal = goal map { ontology.appendTypesTo }

        new Problem(newInit, newGoal, ontology)

      case init ~
        ")" ~ "(" ~ "goal" ~ goal ~ ")" ~ Some("(" ~ "classes" ~ list3 ~ ")") =>
        // the subclass list is supplied and processed below
        // read in subclass information
        val classHash = new HashMap[String, Set[String]]()
        list3 foreach { prop =>
          prop.verb match {
            case 'subclass =>
              val subclass = prop.termlist(0).asInstanceOf[PopObject].name
              val superclass = prop.termlist(1).asInstanceOf[PopObject].name
              classHash.get(superclass) match {
                case Some(set) =>
                  classHash += (superclass -> (set + subclass))
                case None =>
                  classHash += (superclass -> Set(subclass))
              }
            case _ => throw new PopParsingException("invalid propositions in the class section: " + prop)
          }
        }

        // infer all subclasses        
        var changed = true
        while (changed) {
          changed = false
          classHash.keySet foreach { supercl =>
            val base = classHash(supercl)
            var extended = base
            base.foreach { e =>
              val op = classHash.get(e)
              if (op.isDefined) extended = extended ++ op.get
            }

            if (extended.size > base.size) {
              changed = true
              classHash += ((supercl, extended))
            }
          }
        }

        val objectHash = HashMap[String, PopObject]()
        init.foreach(collectTypes(_, objectHash))
        goal.foreach(collectTypes(_, objectHash))

        val ontology = new Ontology(classHash.toMap, objectHash.toMap)
        val newInit = init map { ontology.appendTypesTo }
        val newGoal = goal map { ontology.appendTypesTo }

        new Problem(newInit, newGoal, ontology)
    }

  def collectTypes(prop: Proposition, objectHash: HashMap[String, PopObject]) {
    prop.termlist.foreach {
      _ match {
        case o: PopObject => if (o.pType != "Any") {
          objectHash.get(o.name) foreach
            { storedType =>
              //println(storedType.length + " " + o.pType.length)
              if (storedType.pType != o.pType) throw new PopParsingException("Conflicting types for object: " + o.name + " has type " + storedType + " and " + o.pType)
            }
          objectHash += (o.name -> o)
        }
        case p: Proposition => collectTypes(p, objectHash)
        case v: Variable => throw new PopParsingException("There should not be variables in problem specifications")
      }
    }
  }

  def readFile(file: String): Problem =
    {
      val lines = scala.io.Source.fromFile(file)
      val text = filterComments(lines)
      //println("lines: " + lines)
      val result = parseAll(problem, text)
      result match {
        case Success(x, _) =>
          println("goals :" + x.goal)
          return x
        case NoSuccess(err, next) => {
          println("failed to parse input as planning problem" +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          throw new PopParsingException("planning problem parsing failure.")
        }
      }
    }

  private def filterComments(lines: io.BufferedSource): String =
    {
      var comment = false
      lines.map { char =>
        if (char != '#' && !comment) {
          char
        } else if (char == '#') {
          comment = true
          ' '
        } else if (char == '\n') {
          comment = false
          '\n'
        } else ' '
      }.mkString
    }
}

class PopParsingException(val message: String) extends Exception(message)

object MainParser {
  def main(args: Array[String]) {
    val (prob, actions) = TotalParser.parse("./planfiles/test1.prob", "./planfiles/toyphone.act")
    println(prob.init)
    println(prob.goal)
    println(prob.ontology)
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