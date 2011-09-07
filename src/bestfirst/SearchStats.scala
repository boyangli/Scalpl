package bestfirst

class SearchStats {

  var nodeVisited = 0
  var startTime:Long = 0
  var endTime:Long = 0
  
  override def toString():String = 
    "Statistics: visited " + nodeVisited + " nodes"+ timeUsed.map(" in " + _ + " seconds").getOrElse("")
    
  def start()
  {
    startTime = System.currentTimeMillis()
  }
  
  def finish()
  {
    endTime = System.currentTimeMillis()
  }
  
  /** get total time in seconds
   * 
   */
  def timeUsed:Option[Double] =
  {
    if (endTime == 0 || endTime < startTime) return None
    else Some(((endTime - startTime) / 1000.0))
  }
}