package logging
import plan._
import java.io._

trait Logging {

  lazy val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(Global.debugFile))))

  def debug(message: => String) {

    if (Global.debug == 'debug) {
      val s = message
      println(s)
      if (Global.debugFile != null)
        writer.println(s)
        writer.flush
    }
  }

  def trace(message: => String) {
    if (Global.debug == 'trace || Global.debug == 'debug) {
      val s = message
      println(s)
      if (Global.debugFile != null)
        writer.println(s)
        writer.flush
    }
  }

}