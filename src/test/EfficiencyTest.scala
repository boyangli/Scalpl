package test

/** conclusion: intersect is more expensive to compute
 * 
 */
object EfficiencyTest extends App {

  var time1: Long = 0
  var time2: Long = 0

  for (i <- 0 to 100) {

    val list1 = (0 to 20) map { x => math.round(math.random * 20) } toList
    val list2 = (0 to 20) map { x => math.round(math.random * 20) } toList

    val startTime2 = System.currentTimeMillis()
    for (j <- 0 to 10000) {
      val flag = list2 forall { !list1.contains(_) }
    }
    val endTime2 = System.currentTimeMillis()
    time2 += (endTime2 - startTime2)

    val startTime1 = System.currentTimeMillis()
    for (j <- 0 to 10000) {
      val flag = (list2 intersect list1).isEmpty
    }
    val endTime1 = System.currentTimeMillis()
    time1 += (endTime1 - startTime1)

  }

  println("time 1 = " + time1 / 1000000.0)
  println("time 2 = " + time2 / 1000000.0)

}