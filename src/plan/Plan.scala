package plan
import variable._

case class Plan(
    val id: Int,
    val steps: List[Action],
    val links: List[Link],
    val ordering: Ordering,
    val binding: Binding,
    val flaws: List[Flaw],
    val reason: String,
    val parent: Plan,
    var children: List[Plan]) {

  var stepCount = 0;
  override def toString(): String = "<Plan[" + id + "] #step=" + steps.length + ", #flaws=" + flaws.length + ">"

  def detailString(): String =
    {
      var desc = planString() + "\n"
      desc += "flaws: \n" + flaws.mkString("\n")
      desc += links.mkString("links: \n", "\n", "\n")
      desc += "orderings: \n" + ordering.toString() + "\n"
      desc += "reason: " + reason + "\n"
      desc
    }
  
  def planString():String = 
  {
    var desc = ""
    val order = ordering.topsort() 
     
        for (i <- order if i != 0 && i != Global.GOAL_ID) 
        {
        	steps.find(_.id == i) match
        	{
        	  case Some(x) => desc += "[" + i + "] " + binding.substVars(x)
        	  case _ => ""
        	}
        }
      desc
  }
  
  def id2step(id:Int):Option[Action] = steps.find{_.id == id} 
}

object Plan {
  //  def apply(steps: List[Action], links: List[Link], ordering: Ordering):Plan =
  //  {
  //    // TODO: set the id properly
  //    new Plan(-1, steps, links, ordering, List[Flaw](), "", null)
  //  }

  def getEmpty(): Plan =
    {
      val id = Global.obtainID()
      new Plan(id, List[Action](), List[Link](), new Ordering(), new Binding(), List[Flaw](), "", null, null)
    }
}

class Ordering(val list: List[(Int, Int)]) {

  lazy val allIDs = list.flatMap(x => List(x._1, x._2)).distinct
  
  def this() = this(List[(Int, Int)]())

  def +(x: (Int, Int)) = new Ordering(x :: list)
  
  def topsort(): List[Int] =
    {
      var order = list
      var ans = List[Int]()
      var all = allIDs

      // all nodes that does not have parents in the DAG graph
      var top: List[Int] = all.filterNot(x => order.exists(y => y._2 == x))

      while (top.length > 0) {
        val parent: Int = top.head
        top = top.tail
        //        println("remove " + parent)
        ans = parent :: ans
        val children = order.filter(_._1 == parent).map(_._2)
        for (child <- children) {
          //          println(parent + " 's child = " + child)
          // remove the ordering pair (parent, child) from the order list
          order = order.filterNot(_ == (parent, child))
          // if the child has no more nodes before it
          // add it to top
          if (!order.exists(_._2 == child)) {
            //            println("add " + child + " to top")
            top = child :: top
          }
        }
      }

      if (!order.isEmpty) throw new Exception("The graph contains cycles.")

      ans.reverse
    }

  def nodesBefore(node: Int): List[Int] =
    {
      var order = list
      var ans = List[Int]()
      var all = list.map(x => List(x._1, x._2)).flatten
      var top: List[Int] = order.filter(y => y._2 == node).map(_._1).distinct
      println("top: " + top)
      while (top.length > 0) {

        val cur: Int = top.head
        top = top.tail
        println("remove " + cur)
        ans = cur :: ans

        val parents = order.filter(_._2 == cur).map(_._1)
        println("parents: " + parents)
        order = order.filterNot(_._2 == cur)
        ans = parents ::: ans
        top = (parents ::: top).distinct
      }

      ans.distinct
    }

  def nodesAfter(node: Int): List[Int] =
    {
      var order = list
      var ans = List[Int]()
      var all = list.map(x => List(x._1, x._2)).flatten
      var top: List[Int] = order.filter(y => y._1 == node).map(_._2).distinct
      //      println("top: " + top)
      while (top.length > 0) {

        val cur: Int = top.head
        top = top.tail
        //      println("remove " + cur)
        ans = cur :: ans

        val parents = order.filter(_._1 == cur).map(_._2)
        //    println("parents: " + parents)
        order = order.filterNot(_._1 == cur)
        ans = parents ::: ans
        top = (parents ::: top).distinct
      }

      ans.distinct
    }
  
  def possiblyBefore(node: Int):List[Int] = 
  {
    allIDs filterNot{ nodesAfter(node) contains(_) }
  }
  
  def possiblyAfter(node: Int):List[Int] = 
  {
    allIDs filterNot{ nodesBefore(node) contains(_) }
  }
  
  override def toString():String =
  {
    list map {pair =>
      if(pair._2 == Global.GOAL_ID) (pair._1.toString, "goal")
      else pair
    } mkString(", ")
  }
}

object OrderingTest {
  def main(args: Array[String]) {
    val a = List((1, 2), (1, 3), (1, 5), (5, 2), (4, 2), (4, 5))
    val b = new Ordering(a)
    println(b.topsort())
    println(b.nodesAfter(4))
  }
}