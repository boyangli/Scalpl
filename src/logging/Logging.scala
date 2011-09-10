package logging
import plan._

trait Logging {

  def debug (message: => String)
  {
    if (Global.debug)
      println(message)
  }
}