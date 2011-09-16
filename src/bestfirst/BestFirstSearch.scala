package bestfirst

import scala.collection.mutable.PriorityQueue
import plan._
import logging._

class BestFirstSearch[N] (
    val init: List[N],
    val refine: N => List[N],
    val isComplete: N => Boolean,
    val evaluate: N => Double,
    val parameter: SearchParameter) extends Logging {

  val queue = new PriorityQueue[ValuedNode[N]]()
  val stats = new SearchStats

  def search(): N =
    {
      stats.start()
      val firstNodes = init
      //println("init:" + init.mkString(" "))
      addToQueue(firstNodes)
      while (!queue.isEmpty) {

        // print queue status here
        debug("***\nqueue status: \n" +
          queue.map(node => node.content + " = " + node.value + "\n").mkString)

        val best = queue.dequeue().content
        if (isComplete(best)) {
          stats.finish()
          return best
        }
        
        debug
        {
          val plan = best.asInstanceOf[Plan]
         "refining :" + plan + "\n"  + plan.detailString()
        }
        
        val children = refine(best)
        addToQueue(children)
        if (stats.nodeVisited > parameter.limit)
        {
          stats.finish()
          throw new Exception("Not found: Search limit exceeded")
        }
      }
      // queue is empty but no valid solution found
      stats.finish()
      throw new Exception("Not found: Queue exhausted")
    }

  def addToQueue(nodes: List[N]) {
    for (node <- nodes) {
      val value = evaluate(node)
      queue += new ValuedNode[N](node, evaluate(node))
      stats.nodeVisited += 1
    }
  }
}

object BestFirstSearch {
  def main(args: Array[String]): Unit =
    {
      println("hello")
    }
}