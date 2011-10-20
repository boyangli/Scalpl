package plan

class Record (val oper:String, val stepid:Int, val reason:String) {

  override def toString() = "(" + oper + " " + stepid + ")"
}