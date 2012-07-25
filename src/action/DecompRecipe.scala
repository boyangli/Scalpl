package action

import planning._
import structures._
import variable._

/**
 * decomposition recipes
 *
 */
class RawRecipe(
  val name: String,
  val steps: List[(String, Proposition)],
  val links: List[(String, String, Proposition)],
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
  val steps: List[Action],
  val links: List[Link],
  val ordering: List[(Int, Int)]) {

  override def toString(): String = {
    "[" + name + "]\n" +
      { for (i <- 0 until steps.length) yield "[" + i + "] " + steps(i).toShortString() } +
      "\n " + links.map{_.toString()}.mkString("\n") + ordering.mkString("\norderings: ", ", ", "")

  }

  /*
  def toParseableString(): String =
    {
      "(decomposition " + name +
        "\n  (steps " + steps.map { step => name + " " + p.toShortString() }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (links " + links.map { case (n1, n2, p) => n1 + " -> " + n2 + " " + p.toShortString() }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        "  (ordering " + ordering.map { case (n1, n2) => n1 + " " + n2 }.mkString("\n    (", ") \n    (", ")") + ") \n" +
        ")"
    }
    */
}

/**
 * decomposition links
 *
 */
class DecompLink(val parent: Int,
  val children: List[Int])