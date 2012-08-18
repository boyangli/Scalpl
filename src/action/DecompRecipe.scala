package action

import planning._
import structures._
import variable._
import parsing.Parsible

/**
 * decomposition recipes
 *
 */
class RawRecipe(
  val name: String,
  val steps: List[(Int, Proposition)],
  val links: List[(Int, Int, Proposition)],
  val ordering: List[(String, String)]) {

  def toParseableString(): String =
    {
      "(decomposition " + name +
        "\n  (steps " + steps.map { case (name, p) => name + " " + p.toShortString() }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (links " + links.map { case (n1, n2, p) => n1 + " -> " + n2 + " " + p.toShortString() }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (ordering " + ordering.map { case (n1, n2) => n1 + " " + n2 }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        ")"
    }

}

class Recipe(
  val name: String,
  val steps: List[DecompAction],
  val links: List[Link],
  val ordering: List[(Int, Int)],
  val bindings: List[(Variable, Token)]) extends Parsible {

  override def toString(): String = {
    "[" + name + "]\n" +
      { for (i <- 0 until steps.length) yield "[" + i + "] " + steps(i).toShortString() } +
      "\n " + links.map { _.toString() }.mkString("\n") + ordering.mkString("\norderings: ", ", ", "")

  }

  override def toParseString(): String =
    {
      "(decomposition " + name +
        "\n  (steps " +
        {
          for (i <- 0 until steps.length) yield {
            val s = steps(i)
            "step" + i + " " + s.toShortString()
          }
        }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (links " + links.map { link =>
          "step" + link.id1 + " -> step" + link.id2 + " " +
            link.precondition.toShortString()
        }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (ordering " + ordering.map { case (n1, n2) => "step" + n1 + " step" + n2 }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        ")"
    }
}

/**
 * decomposition links
 *
 */
class DecompLink(val parent: Int,
  val children: List[Int])