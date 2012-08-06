package variable

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import planning._
import structures._

class VarSet(val equals: List[Token], val nonEquals: List[Token], val groundObj: PopObject, val pType: String) {

  //private var grounded = false

  def bindTo(token: Token)(implicit ontology: Ontology): VarSet = token match {
    case sym: PopObject =>
      {
        if (isGrounded) {
          // varset already bound to a symbol
          if (groundObj == sym) return this // already bound to this symbol, do nothing
          else throw new BindingException("varset already bound to a symbol: " + groundObj + " different from " + sym)
          // cannot bound to a different symbol, throw an exception
        } else if (nonEquals.contains(sym)) throw new BindingException("varset already bound to the negation of the symbol: " + sym)
        else {
          ontology.lower(pType, sym.pType) match {
            case Some(lowerType) =>
              new VarSet(equals, nonEquals, sym, lowerType)
            case None => throw new BindingException("symbol " + sym + " is not a " + sym.pType)
          }

        }
      }

    case v: Variable =>
      {
        if (nonEquals.contains(v))
          throw new Exception("varset already bound to the negation of the variable: " + v)
        else if (equals.contains(v))
          this // already bound to the current variable, do nothing
        else {
          ontology.lower(pType, v.pType) match {
            case Some(lowerType) =>
              new VarSet(v :: equals, nonEquals, groundObj, lowerType)
            case None => throw new BindingException("variable " + v + " is not a " + v.pType)
          }
        }
      }
  }

  def bindNotTo(token: Token): VarSet = token match {
    case sym: PopObject =>
      {
        if (sym == groundObj) throw new BindingException("already bound to the symbol. Cannot bind to its negation")
        else new VarSet(equals, sym :: nonEquals, groundObj, pType)
      }

    case v: Variable =>
      {
        if (equals.contains(v)) throw new BindingException("already bound to the variable. Cannot bind to its negation")
        else new VarSet(equals, v :: nonEquals, groundObj, pType)
      }
  }

  def bindNotTo(tokens: List[Token]): VarSet = {
    var newNonEquals = this.nonEquals
    tokens foreach {
      _ match {

        case sym: PopObject =>
          {
            if (sym == groundObj) throw new BindingException("already bound to the symbol. Cannot bind to its negation")
            else newNonEquals ::= sym
          }

        case v: Variable =>
          {
            if (equals.contains(v)) throw new BindingException("already bound to the variable. Cannot bind to its negation")
            else newNonEquals ::= v
          }
      }
    }
    new VarSet(equals, newNonEquals, groundObj, pType)
  }

  override def equals(a: Any): Boolean = a match {
    case that: VarSet =>
      {
        if (this eq that) true // performance shortcut. Reference comparison
        else (that.canEqual(this) && this.equals == that.equals && this.nonEquals == that.nonEquals && this.pType == that.pType)
      }

    case _ => false
  }

  def canEqual(a: Any): Boolean = a match {
    case that: VarSet => true
    case _ => false
  }

  def isGrounded() = (groundObj != null)

  def contains(v: Variable): Boolean = equals.contains(v)

  def contains(s: PopObject): Boolean = groundObj == s

  override def toString() = equals.mkString("<varset: (", ", ", ")" +
    (if (isGrounded()) "=" + groundObj else "") +
    "; ") +
    (if (nonEquals != Nil) nonEquals.mkString("non-equal: (", ", ", ")")) + ">"

  override def hashCode(): Int =
    {
      ((equals.hashCode + 199) * 199 + (nonEquals.hashCode + 157) * 157) % 97 + 97
    }

  def isCompatibleWith(that: VarSet)(implicit ontology:Ontology): Boolean =
    {
      that.equals.forall(!this.nonEquals.contains(_)) && // none of the nonequals is contained in the equals 
        this.equals.forall(!that.nonEquals.contains(_)) &&
        (!(this.isGrounded() && that.isGrounded() && this.groundObj != that.groundObj)) && // each is bounded to a different symbol: failure
        ontology.compatible(this.pType, that.pType)
    }

  /**
   * warning: this methods does not check for incompatiability. The user must check it before applying this method
   * merges one varset with another varset
   */
  def mergeWith(that: VarSet)(implicit ontology: Ontology): VarSet =
    {
      new VarSet(this.equals ::: that.equals, this.nonEquals ::: that.nonEquals,
        if (this.isGrounded()) this.groundObj else that.groundObj, ontology.lower(this.pType, that.pType).get)
    }

  def equalsAndSymbol() = if (isGrounded()) groundObj :: equals else equals
}

object VarSet {

  //  def apply(s: Symbol, vars: Variable*): VarSet =
  //    {
  //      new VarSet(vars.toList, List[Variable](), s)
  //    }

  def apply(token: Token): VarSet = token match {
    case v: Variable => VarSet.apply(v)
    case o: PopObject => VarSet.apply(o)
  }

  def apply(variable: Variable): VarSet = {
    new VarSet(List(variable), List[Variable](), null, variable.pType)
  }

  def apply(obj: PopObject): VarSet = {
    new VarSet(List[Variable](), List[Variable](), obj, obj.pType)
  }

  def apply(s: PopObject, vars: Variable*)(implicit ontology: Ontology): VarSet = {

    val varList = vars.toList
    val pType: String =
      varList.foldLeft("Any") { (x: String, y: Variable) => ontology.lower(x, y.pType).get }
    new VarSet(varList, List[Variable](), s, pType)
  }

  //  def apply(vars: Variable*): VarSet =
  //    {
  //      new VarSet(vars.toList, List[Variable](), null)
  //    }
  //

  def apply(tokens: Token*)(implicit ontology: Ontology): VarSet =
    {
      val tlist = tokens.toList
      var symbols: List[PopObject] = List[PopObject]()
      var vars: List[Variable] = List[Variable]()

      //      val (vars: List[Variable], symbols: List[PopObject]) = tlist.span(_.isInstanceOf[Variable])
      val pType: String =
        tlist.foldLeft("Any") { (x: String, y: Token) => ontology.lower(x, y.pType).get }

      tlist.foreach {
        _ match {
          case v: Variable => vars ::= v
          case s: PopObject => symbols ::= s
        }
      }

      symbols.length match {
        case 0 =>
          {
            new VarSet(vars, List[Token](), null, pType)
          }
        case 1 =>
          {
            new VarSet(vars, List[Token](), symbols(0).asInstanceOf[PopObject], pType)
          }
        case _ =>
          {
            throw new BindingException("A VarSet cannot contain two or more symbols: " + symbols.mkString(", "))
          }
      }

    }
}

class BindingException(msg: String) extends Exception(msg)

object VarSet_Test {
  // testing code
  //  def main(args: Array[String]) {
  //    val p1 = Proposition.parse("(kick adam bob ?o1 ?k1)")
  //    val p2 = Proposition.parse("(kick adam ?p2 ?i2 ?i3)")
  //    println(p1)
  //    println(p2)
  //    var b = new Binding()
  //    //    b += VarSet('alice, Variable("?i3"))
  //    b += VarSet(Variable("?i2"), 'cathy)
  //
  //    val matched = b.matchTokens(p1, p2)
  //    println(matched)
  //    if (matched isEmpty) return // fail to match the two propositions
  //    // step 2: retrieve known values for variables that are already bound
  //    val valued = b.revealTokenValues(matched.get)
  //    println(valued)
  //    // step 4: check for constraints
  //    val const = List(Proposition.parse("(person ?p2)"), Proposition.parse("(person ?o1)"))
  //    val initial = List(Proposition.parse("(person bob)"), Proposition.parse("(person adam)"))
  //    val newb = b.unifyWithConstraints(valued, const, initial)
  //    if (newb.isEmpty)
  //      println("binding fails")
  //    else {
  //      val newbind = newb.get
  //      val newp1 = newbind.substVars(p1)
  //      val newp2 = newbind.substVars(p2)
  //      println(p1 + " => " + newp1)
  //      println(p2 + " => " + newp2)
  //      println(newp1.equalsIgnoreVars(newp2))
  //    }
  //  }
  def main(args: Array[String]) {
    val g = new GlobalInfo(Nil, null)
    val p1 = Proposition.parse("(kick jack tom ?p2 ?p3)")
    val p2 = Proposition.parse("(kick jack ?p4 jill ?p6)")
    var bind = new Binding()
    //bind += VarSet(Variable("?p4"), 'tom)
    //bind += VarSet(Variable("?p2"), 'adam)
    val list = bind.separate(p1, p2, g)
    //	  val b = list(0)
    //	  println(b.hashes.keySet)
    println(list.mkString("\n\n"))
  }

}
