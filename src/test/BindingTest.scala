package test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import variable._
import plan._

class BindingTest extends AssertionsForJUnit {

  val p1 = Proposition.parse("(kick jack tom ?p2 ?p3)")
  val p2 = Proposition.parse("(kick jack ?p4 jill ?p6)")

  @Before def initialize() {
  }

  @Test def alreadyDifferent() { // Uses JUnit-style assertions
    var bind = new Binding()
    bind += VarSet('tom, Variable("?p4"))
    bind += VarSet('adam, Variable("?p2"))
    val list = bind.separate(p1, p2, List(), List())
    //	  val b = list(0)
    //	  println(b.hashes.keySet)
    println(list.mkString("\n\n"))
    //	  print(list(0).hashes.get(Variable("?p3")))
    assertEquals(list(0).hashes.get(Variable("?p3")), None)
  }

  @Test def simpleSeparate() { 
    var bind = new Binding()
    bind += VarSet('jill, Variable("?p2"))
    val list = bind.separate(p1, p2, List(), List())
    //	  val b = list(0)
    //	  println(b.hashes.keySet)
    println(list.mkString("\n\n"))

    // ?p4 is separated from tom
    assert(list exists { x =>
      !(x.get(Variable("?p4")) filter { _.nonEquals == List('tom) } isEmpty)
    })

    // ?p6 is separated from ?p3
    assert(list exists { x =>
      !(x.get(Variable("?p6")) filter { _.nonEquals == List(Variable("?p3")) } isEmpty)
    })   
  }
  
  @Test def cannotSeparate() { 
    var bind = new Binding()
    bind += VarSet('jill, Variable("?p2"))
    bind += VarSet('tom, Variable("?p4"))
    bind += VarSet('adam, Variable("?p3"), Variable("?p3"))
    val list = bind.separate(p1, p2, List(), List())
    assertEquals(list, None)
}