package test

case class DefaultValue (val a:Int, val b:Int = 2) {

}

object Main {
  def main(args:Array[String])
  {
    println(new DefaultValue(1, 3))
    println(new DefaultValue(1))
  }
}