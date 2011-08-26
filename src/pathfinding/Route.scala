package pathfinding
import bestfirst._

class Route(val list: List[Int]) {
  def append(next: Int): Route =
    {
      new Route(list ::: List(next))
    }

  override def toString(): String =
    {
      "<Route: " + list.mkString(" ") + ">"
    }
}

object Route {

  var distance = Array.fill(5, 5)(0.0)

  setDist(0, 1, 5)
  setDist(0, 2, 10)
  setDist(0, 3, 3)
  setDist(0, 4, 1000)
  setDist(1, 2, 8)
  setDist(1, 3, 50)
  setDist(1, 4, 5)
  setDist(2, 3, 12)
  setDist(2, 4, 6)
  setDist(3, 4, 15)

  def setDist(x: Int, y: Int, dist: Double) {
    distance(x)(y) = dist
    distance(y)(x) = dist
  }

  def print() {
    val string = distance.map(_.mkString(" ")).mkString("\n")
    println(string)
  }

  def main(args: Array[String]) {
    val init = (0 to 4).map(x => new Route(List(x))).toList

    val refine: Route => List[Route] = route =>
      {
        (0 to 4).filter(x => !route.list.contains(x)).map(x => 
          {
            route.append(x)
          }).toList
      }

    val isComplete: Route => Boolean = route =>
      {
        route.list.contains(0) && route.list.contains(1) && route.list.contains(2) && 
        route.list.contains(3) && route.list.contains(4)
      }

    val evaluate: Route => Double = route =>
      {
        var start = route.list.head
        var sum = 0.0
        for (i <- route.list.tail)
        {
          sum += distance(start)(i)
          start = i
        }
        2000 - sum
      }
      
      val parameter = new SearchParameter(1000)
      
    val best = new BestFirstSearch[Route](init, refine, isComplete, evaluate, parameter)
    val result = best.search
    
    println("Result :" + result.toString)
  }
}