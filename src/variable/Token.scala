package variable

import parsing._

sealed abstract class TopTerm {
  override def equals(o: Any): Boolean

  override def hashCode(): Int

  def canEqual(o: Any): Boolean
}

case class Proposition(val verb: PopSymbol, val termlist: TermList) extends TopTerm {
  
  //def this(verb: PopSymbol, list: List[TopTerm]) = this(verb, TermList(list))

  def length = termlist.length + 1
  override def toString(): String = {
    if (termlist.list.length == 0)
      "Prop: " + verb
    else (verb + "(" + termlist.toString + ")").substring(2)
  }

  override def equals(o: Any): Boolean = o match {
    case that: Proposition => that.canEqual(this) && this.verb == that.verb && this.termlist == that.termlist
    case _ => false
  }

  override def hashCode(): Int = (termlist.hashCode * 37 + verb.hashCode * 31) % 29

  def canEqual(o: Any): Boolean = o match {
    case that: Proposition => true
    case _ => false
  }

  def allVariables(): List[Variable] =
    {
      var list = List[Variable]()

      termlist.foreach(_ match {
        case v: Variable => list = v :: list
        case p: Proposition => list = list ::: p.allVariables()
        case _ =>
      })

      list
    }

  def substitute(v: Variable, s: PopSymbol): Proposition =
    {
      Proposition(verb, termlist.map(x => if (x == v) s else x))
    }

  def equalsIgnoreVars(that: Proposition): Boolean =
    {
      this.verb == that.verb && this.length == that.length &&
        (this.termlist, that.termlist).zipped.forall {
          (_, _) match {
            // all cases of the two matching pairs of the termlists
            case (v1: Variable, t: Token) => true
            case (t: Token, v2: Variable) => true
            case (p1: Proposition, p2: Proposition) => p1.equalsIgnoreVars(p2)
            case (s1: PopSymbol, s2: PopSymbol) => s1 == s2
            case _ => false
          }
        }
    }
  
  def instantiate(number:Int):Proposition =
  {
    new Proposition(verb, termlist map { _ match
      {
      case v:Variable => v.instantiate(number)
      case p:Proposition => p.instantiate(number)
      case x => x
      }
    })
  }
}

object Proposition {
  def parse(string: String): Proposition = ActionParser.parseAll(ActionParser.prop, string).get

}

case class TermList(val list: List[TopTerm]) extends TopTerm {
  override def toString(): String =
    {
      list.mkString(" ")
    }

  override def equals(o: Any): Boolean = o match {
    case that: TermList => that.canEqual(this) && this.list == that.list
    case _ => false
  }

  override def hashCode(): Int = (list.hashCode + 97) % 97

  def canEqual(o: Any): Boolean = o match {
    case that: TermList => true
    case _ => false
  }
}

object TermList {
  implicit def tlToList(tl: TermList): List[TopTerm] = tl.list
  implicit def listToTermList(list: List[TopTerm]): TermList = TermList(list)
}

/**
 * Token class. Super class of PopSymbol and Variable
 *
 */
sealed abstract class Token extends TopTerm

/**
 * Basic unit. Represents a symbol.
 *
 */
case class PopSymbol(val symbol: Symbol) extends Token {
  override def equals(o: Any): Boolean = o match {
    case that: PopSymbol => that.canEqual(this) && this.symbol == that.symbol
    case that: Symbol => this.symbol == that
    case _ => false
  }

  override def hashCode() = symbol.hashCode()

  def canEqual(o: Any): Boolean = o match {
    case that: PopSymbol => true
    case _ => false
  }

  override def toString(): String = "'" + symbol

}

object PopSymbol {
  implicit def symbolToPop(s: Symbol): PopSymbol = PopSymbol(s)
  implicit def popToSymbol(ps: PopSymbol): Symbol = ps.symbol
}

/**
 * a basic unit that represents a variable
 *
 */
case class Variable(val name: String, val number: Int) extends Token {

  def this(name: String) = this(name, 0)

  override def toString: String =
    if (number == 0) "v-" + name
    else "v-" + name + "-" + number

  def instantiate(number: Int) = new Variable(name, number)

  override def equals(o: Any): Boolean = o match {
    case that: Variable => that.canEqual(this) && (this.name == that.name) && (this.number == that.number)
    case _ => false
  }

  override def hashCode() = (toString().hashCode() * 97 + number * 89) % 37

  def canEqual(o: Any): Boolean = o match {
    case that: Variable => true
    case _ => false
  }

  def bindTo(s: Symbol): VarSet = VarSet(s, this)
  def bindTo(vs: VarSet) = vs.bindTo(this)

}

object Variable {
  def apply(name: String) = new Variable(name)
}