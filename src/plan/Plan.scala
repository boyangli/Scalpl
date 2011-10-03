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
  var children: List[Plan],
  val stepCount: Int = 0) {

  //var stepCount = 0;
  override def toString(): String = "<Plan[" + id + "] #steps=" + stepCount + ", #flaws=" + flaws.length + ">"

  def detailString(): String =
    {
      var desc = planString() + "\n"
      desc += flaws.mkString("flaws: \n", "\n", "\n")
      desc += links.mkString("links: \n", "\n", "\n")
      desc += "orderings: \n" + ordering.toString() + "\n"
      desc += "reason: " + reason + "\n"
      desc
    }

  def planString(): String =
    {
      // a plan without steps
      if (steps.length <= 2) return "Plan[" + id + "] with 0 steps"

      var desc = ""
      //println(ordering.allIDs)
      val order = ordering.topsort()
      //print("order "+ order)
      for (i <- order if i != 0 && i != Global.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => desc += "[" + i + "] " + binding.substVarsString(x) + "\n"
          case _ => ""
        }
      }
      desc
    }

  def parsibleString(): String =
    {
      var answer = "(objects " + collectObjects.mkString(" ") + ")\n" // objects
      answer += "(initial-state " + initialState.map(_.toShortString).mkString(" ") + ")\n" // initial state
      
      val order = ordering.topsort()
      var stepString = ""
        
      for (i <- order if i != 0 && i != Global.GOAL_ID) {
        steps.find(_.id == i) match {
          case Some(x) => stepString += "[" + i + "] " + binding.substVarsShortString(x) + "\n"
          case _ => ""
        }
      }
      
      answer +=  "(goal-state " + goalState.map(_.toShortString).mkString(" ") + ")\n"
      answer += "(steps \n" + stepString + ")\n"
      answer += "(links " + links.map(_.toFileString).mkString("\n") + ")\n" 
      answer += "(orderings " + ordering.toFileString() + ")\n"

      answer
    }

  private def collectObjects(): Set[PopObject] =
    {
      var collection = Set[PopObject]()
      // collect objects from initial / goal states
      (initialState ::: goalState) foreach { prop =>
        collection ++= prop.allObjects()
      }

      // collect objects from all steps
      steps foreach {
        _.parameters foreach {
        	_ match
        	{
        	  case o:PopObject => collection += o
        	  case _ =>
        	}
        }
      }

      collection
    }

  def id2step(id: Int): Option[Action] = steps.find { _.id == id }

  def initialState() = steps.find(_.id == 0).get.effects // initial state
  def goalState() = steps.find(_.id == Global.GOAL_ID).get.preconditions // goal state
}

object Plan {

  def getEmpty(): Plan =
    {
      val id = Global.newPlanID()
      new Plan(id, List[Action](), List[Link](), new Ordering(), new Binding(), List[Flaw](), "", null, null)
    }
}

class Ordering(val list: Set[(Int, Int)]) {

  lazy val allIDs: Set[Int] =
    {
      var all = List[Int]()
      list.foreach(x => all = x._1 :: x._2 :: all)
      all.toSet[Int]
    }

  def this() = this(Set[(Int, Int)]())

  def +(x: (Int, Int)) = new Ordering(list + x)

  def topsort(): List[Int] =
    {
      var order = list
      var ans = List[Int]()
      var all = allIDs

      // all nodes that does not have parents in the DAG graph
      var top: Set[Int] = all.filterNot(x => order.exists(y => y._2 == x))

      while (top.size > 0) {
        //print("top:" + top)
        val parent: Int = top.head
        top = top.tail
        //println("remove " + parent)
        ans = parent :: ans
        val children = order.filter(_._1 == parent).map(_._2)
        for (child <- children) {
          //          println(parent + " 's child = " + child)
          // remove the ordering pair (parent, child) from the order list
          order = order.filterNot(_ == (parent, child))
          // if the child has no more nodes before it
          // then: add it to top
          if (!order.exists(_._2 == child)) {
            //            println("add " + child + " to top")
            top += child
          }
        }
      }

      if (!order.isEmpty) throw new Exception("The graph contains cycles.")

      ans.reverse
    }

  def nodesBefore(node: Int): Set[Int] =
    {
      var order = list
      var ans = Set[Int]()
      var all = allIDs
      var top: Set[Int] = order.filter(y => y._2 == node).map(_._1)
      //println("top: " + top)
      while (top.size > 0) {

        val cur: Int = top.head
        top = top.tail
        //println("remove " + cur)
        ans = ans + cur

        val parents = order.filter(_._2 == cur).map(_._1)
        //println("parents: " + parents)
        order = order.filterNot(_._2 == cur)
        //ans = parents ++ ans
        top = parents ++ top
      }

      ans
    }

  def nodesAfter(node: Int): Set[Int] =
    {
      var order = list
      var ans = Set[Int]()
      var all = allIDs
      var top: Set[Int] = order.filter(y => y._1 == node).map(_._2)
      //      println("top: " + top)
      while (top.size > 0) {

        val cur: Int = top.head
        top = top.tail
        //      println("remove " + cur)
        ans += cur

        val parents = order.filter(_._1 == cur).map(_._2)
        //    println("parents: " + parents)
        order = order.filterNot(_._1 == cur)
        //ans = parents ++ ans
        top = parents ++ top
      }

      ans
    }

  def possiblyBefore(node: Int): Set[Int] =
    {
      allIDs filterNot { x => nodesAfter(node).contains(x) || x == node }
    }

  def possiblyAfter(node: Int): Set[Int] =
    {
      allIDs filterNot { x => nodesBefore(node).contains(x) || x == node }
    }

  /**
   * Returns a set of temporal orderings where none is implied by other orderings
   * This is a simple implementation and do not guarantee to return the minimal set
   *
   */
  def necessary(): Set[(Int, Int)] =
    {
      val store = new scala.collection.mutable.HashSet[(Int, Int)]()
      //println("list=" + list)
      var head = list.head
      var tail = list.toList
      var rest = tail
      //println("rest :" + rest)
      while (tail != Nil) {
        tail = tail.tail
        //println("checking head " + head)
        // check if the head element is necessary
        val after = Ordering.idsAfter(head._1, rest)
        //println("after = " + after)
        if (after contains head._2) {
          // head is not necessary
          //println("not needed")
        } else {
          // head is necesssary
          //println("needed")
          store += head
          rest = head :: rest
        }
        if (tail != Nil) {
          head = tail.head
          rest = rest - head
          //println("Rest: " + rest)
        }
      }
      store.toSet
    }

  override def toString(): String =
    {
      list map { pair =>
        if (pair._2 == Global.GOAL_ID) (pair._1.toString, "goal")
        else pair
      } mkString (", ")
    }
  
    def toFileString(): String =
    {
      list map { pair =>
        if (pair._2 == Global.GOAL_ID) (pair._1.toString, "goal")
        else pair
      } mkString (" ")
    }

  //def allNodes()
}

object Ordering {
  def idsAfter(node: Int, ordering: List[(Int, Int)]): Set[Int] =
    {
      var order = ordering
      var ans = List[Int]()
      var all = List[Int]()

      ordering.foreach(x => all = x._1 :: x._2 :: all)
      all = all.distinct

      var top: List[Int] = order.filter(y => y._1 == node).map(_._2)
      //println("top: " + top)
      while (top.size > 0) {

        val cur: Int = top.head
        top = top.tail
        //println("remove " + cur)
        ans = cur :: ans

        val parents = order.filter(_._1 == cur).map(_._2)
        //println("parents: " + parents)
        order = order.filterNot(_._1 == cur)
        // ans = parents ::: ans
        top = (parents ::: top).distinct
      }

      ans.toSet[Int]
    }
}

object OrderingTest {
  def main(args: Array[String]) {
    val a = Set((1, 2), (1, 3), (1, 5), (5, 2), (4, 2), (4, 5), (5, 6), (0, 1), (6, 7))
    val b = new Ordering(a)
    println(b.topsort())
    println(b.nodesBefore(6))

    var nec = b.necessary()
    println(nec)
    val c = new Ordering(nec)
    println(c.topsort())
    println(c.nodesBefore(6))

    var bool = true
    for (i <- 0 to 7) {
      bool = bool && c.nodesBefore(i) == b.nodesBefore(i)
    }
    print(bool)
  }
}