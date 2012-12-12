package agagame

import bestfirst._
import scala.collection.mutable.PriorityQueue
import scala.collection.mutable.ListBuffer
import planning._
import structures._

class ExhaustiveSearch[N](
  init: List[N],
  refine: N => List[N],
  isComplete: N => Boolean,
  evaluate: N => Double,
  parameter: SearchParameter) extends BestFirstSearch[N](init, refine, isComplete, evaluate, parameter) {

  def exhaustiveSearch(max: Int): List[N] =
    {
      var solutions = ListBuffer[N]()
      var count = 1

      val firstNodes = init
      //println("init:" + init.mkString(" "))
      addToQueue(firstNodes)
      while (!queue.isEmpty && count <= max) {

        // print queue status here
        debug("***\nqueue status: \n" +
          queue.map(node => node.content + " = " + node.value + "\n").mkString)

        val best = queue.dequeue().content

        if (isComplete(best)) {
          solutions += best
        } else {

          debug {
            val plan = best.asInstanceOf[Plan]

            "refining :" + plan + "\n" + plan.detailString()
          }

          val children = refine(best)
          addToQueue(children)
          count += children.size
        }
      }

      solutions.toList
    }

}