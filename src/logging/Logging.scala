package logging
import planning._
import java.io._

trait Logging {
 
  lazy val writer = new PrintWriter(new BufferedOutputStream(new FileOutputStream(new File(DebugInfo.debugFile))))

  def debug(message: => String) {

    if (DebugInfo.debug == 'debug) {
      val s = message
      println(s)
      if (DebugInfo.debugFile != null)
        writer.println(s)
        writer.flush
    }
  }

  def trace(message: => String) {
    if (DebugInfo.debug == 'trace || DebugInfo.debug == 'debug) {
      val s = message
      println(s)
      if (DebugInfo.debugFile != null)
        writer.println(s)
        writer.flush
    }
  }

}

object DebugInfo {
  protected var debugLvl = 'normal
  val debugFile = "log.txt"

  def setNormal() {
    debugLvl = 'normal
  }

  def setTrace() {
    debugLvl = 'trace
  }

  def setDebug() {
    debugLvl = 'debug
  }

  def debug() = debugLvl
}