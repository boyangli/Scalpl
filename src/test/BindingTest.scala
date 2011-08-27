package test
import org.scalatest.junit.AssertionsForJUnit
import org.scalatest.junit.JUnitRunner
import scala.collection.mutable.ListBuffer
import org.junit.Assert._
import org.junit.Test
import org.junit.Before
import variable._
import variable.PopSymbol._
import plan._

class BindingTest extends AssertionsForJUnit {

  val p1 = Proposition.parse("(kick jack tom ?p2 ?p3)")
  val p2 = Proposition.parse("(kick jack ?p4 jill ?p6)")

  @Before def initialize() {
  }

  @Test def alreadyDifferent() { // Uses JUnit-style assertions
    var bind = new Binding()
    bind += VarSet(Variable("?p4"), 'tom)
    bind += VarSet(Variable("?p2"), 'adam)
    val list = bind.separate(p1, p2)
    //	  val b = list(0)
    //	  println(b.hashes.keySet)
    //    println(list.mkString("\n\n"))
    //    	  print(list(0).hashes.get(Variable("?p3")))
    assertEquals(list(0).hashes.get(Variable("?p3")), None)
  }

  @Test def simpleSeparate() {
    var bind = new Binding()
    bind += VarSet('jill, Variable("?p2"))
    val list = bind.separate(p1, p2)
    //	  val b = list(0)
    //	  println(b.hashes.keySet)
    //    println(list.mkString("\n\n"))
    //
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
    bind += VarSet('adam, Variable("?p3"), Variable("?p6"))
    val list = bind.separate(p1, p2)
    //    print(list)
    assertEquals(list, Nil)
  }

  @Test def embededProps1() {
    val p3 = Proposition.parse("(knows jack (kill jill ?p2))")
    val p4 = Proposition.parse("(knows ?p1 (kill ?p5 ?p4))")
    var bind = new Binding()
    bind += VarSet('jill, Variable("?p5"))
    val list = bind.separate(p3, p4)

    // ?p4 is separated from tom
    assert(list exists { x =>
      !(x.get(Variable("?p1")) filter { _.nonEquals == List('jack) } isEmpty)
    })

    // ?p6 is separated from ?p3
    assert(list exists { x =>
      !(x.get(Variable("?p2")) filter { _.nonEquals == List(Variable("?p4")) } isEmpty)
    })
  }

  @Test def embededProps2() {
    val p3 = Proposition.parse("(knows jack (kill jill ?p2))")
    val p4 = Proposition.parse("(knows ?p1 (kill ?p5 ?p4))")
    var bind = new Binding()
    bind += VarSet(Variable("?p5"), 'jill)
    bind += VarSet(Variable("?p4"), 'tom)
    val list = bind.separate(p3, p4)

    // ?p4 is separated from jack
    assert(list exists { x =>
      !(x.get(Variable("?p1")) filter { _.nonEquals == List('jack) } isEmpty)
    })

    // ?p2 is separated from ?p4 and tom
    assert(list exists { x =>
      !(x.get(Variable("?p2")) filter { x => x.nonEquals.contains(Variable("?p4")) && x.nonEquals.contains('tom) } isEmpty)
    })
  }
}