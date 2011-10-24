package plan

class Record (val oper:String, val stepid:Int, val reason:String, val value:Double) {

  def this(oper:String, stepid:Int, reason:String) = this(oper, stepid, reason, 0)
  
  override def toString() = "(" + oper + " " + stepid + ")"
}