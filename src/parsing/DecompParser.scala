package parsing

import action._
import variable._
import scala.util.parsing.combinator._
import planning._
import structures._

object DecompParser extends PopParser {

  var actions: List[Action] = null

  def readFile(file: String, actionList: List[Action]): List[RawRecipe] =
    {
      actions = actionList
      val lines = scala.io.Source.fromFile(file).mkString
      //println("lines: " + lines)
      // have to parse each action separately
      val result = parseAll(recipeList, lines)

      result match {
        case Success(x, _) => return x
        case NoSuccess(err, next) => {
          println("failed to parse input as decomposition recipes" +
            "(line " + next.pos.line + ", column " + next.pos.column + "):\n" +
            err + "\n" +
            next.pos.longString)
          throw new PopParsingException("decomposition parsing failure.")
        }
      }
    }

  def recipeList: Parser[List[RawRecipe]] = rep("""\s*""".r ~ recipe) ^^
    {
      case list =>
        list.map(x => x._2)
    }

  def recipe: Parser[RawRecipe] =
    "(" ~ "decomposition" ~> string ~ "(" ~ "steps" ~ rep(recipeStep) ~ ")" ~
      "(" ~ "links" ~ rep(recipeLink) ~ ")" ~
      "(" ~ "ordering" ~ rep(recipeOrdering) <~ ")" ~ ")" ^^
      {
        case name ~ "(" ~ "steps" ~ steps ~ ")" ~
          "(" ~ "links" ~ links ~ ")" ~
          "(" ~ "ordering" ~ orderings =>
          new RawRecipe(name, steps, links, orderings)
      }

  def recipeStep: Parser[(String, Proposition)] = "(" ~> string ~ prop <~ ")" ^^
    {
      case name ~ p => (name, p)
    }

  def recipeLink: Parser[(String, String, Proposition)] = "(" ~> string ~ "->" ~ string ~ prop <~ ")" ^^
    {
      case name1 ~ "->" ~ name2 ~ p => (name1, name2, p)
    }

  def recipeOrdering: Parser[(String, String)] = "(" ~> string ~ string <~ ")" ^^
    {
      case name1 ~ name2 => (name1, name2)
    }

}

object TestRecipeReader extends App {
  logging.DebugInfo.setDebug()
  val file = "./planfiles/testDecomp.decomp"
  val (problem, actions, recipes) = TotalParser.decompParse("./planfiles/dpocl1.prob", "./planfiles/dpocl1.act", "./planfiles/dpocl1.decomp")
  recipes.foreach(r => println(r.toString()))
}