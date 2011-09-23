package variable

import scala.collection.mutable.HashMap
import logging._
import scala.collection.mutable.ListBuffer
import plan._

class Binding private (val hashes: HashMap[Token, VarSet]) extends Logging {
  //  var varsets = List[VarSet]()
  //  val hashes = new HashMap[Variable, VarSet]()
  // 
  def this() = this(new HashMap[Token, VarSet]())

  def +(vs: VarSet): Binding = {
    val bind = new Binding(hashes.clone())
    vs.equals.foreach(v => bind.hashes += (v -> vs))
    if (vs.isGrounded())
      bind.hashes += (vs.groundObj -> vs)
    bind
  }

  def +(vss: VarSet*): Binding = {
    val bind = new Binding(hashes.clone())

    vss foreach { vs =>
      vs.equals.foreach(v => bind.hashes += (v -> vs))
      if (vs.isGrounded())
        bind.hashes += (vs.groundObj -> vs)
    }
    bind
  }

  def get(token: Token) = hashes.get(token)

  override def toString(): String =
    {
      hashes.values.mkString("\n")
    }

  /**
   * attempts to build a consistent variable binding that 1) make p1 and p2 equivalent,
   * and 2) respects the given constraints.
   *
   */
  def unify(p1: Proposition, p2: Proposition, constraints: List[Proposition], initial: List[Proposition]): Option[Binding] =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2)
      if (matched isEmpty) return None // fail to match the two propositions
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: check for superficial inconsistencies. If there is, return None
      if (!isPairConsistent(valued)) return None
      // step 4: check for constraints
      unifyWithConstraints(valued, constraints, initial)
    }

  /**
   * superficially tests for unification
   *
   */
  def canUnify(p1: Proposition, p2: Proposition): Boolean =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2)
      if (matched isEmpty) return false // fail to match the two propositions
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: check for superficial inconsistencies. If there is, return None
      if (!isPairConsistent(valued)) return false
      else return true
    }

  /**
   * directly unify without testing for superficial inconsistency
   * still tests for consistency with constraints
   *
   */
  def directUnify(p1: Proposition, p2: Proposition, constraints: List[Proposition], initial: List[Proposition]): Option[Binding] =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2)
      val valued = revealTokenValues(matched.get)
      unifyWithConstraints(valued, constraints, initial)
    }

  def addNeqs(neqs: List[Proposition]): Binding =
    {
      var newbind = this
      neqs foreach { neq =>
        val item1 = neq.termlist(0).asInstanceOf[Token]
        val item2 = neq.termlist(1).asInstanceOf[Token]

        (get(item1), get(item2)) match {
          case (Some(vs1: VarSet), Some(vs2: VarSet)) =>
            newbind += (vs1.bindNotTo(vs2.equals), vs2.bindNotTo(vs1.equals))
          case (Some(vs1: VarSet), None) =>
            newbind += (vs1.bindNotTo(item2), VarSet(item2).bindNotTo(item1))
          case (None, Some(vs2: VarSet)) =>
            newbind += (VarSet(item1).bindNotTo(item2), vs2.bindNotTo(item1))
          case (None, None) =>
            newbind += (VarSet(item1).bindNotTo(item2), VarSet(item2).bindNotTo(item1))
        }
      }
      newbind
    }

  /**
   * attempts to build a set of consistent variable binding that 1) make p1 and p2 different,
   * and 2) respects the given constraints.
   *
   */
  def separate(p1: Proposition, p2: Proposition): List[Binding] =
    {
      // step 0: if they are already the same, no separation is possible and return Nil
      if (substVars(p1) == substVars(p2)) return Nil
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2)
      if (matched isEmpty) return List(this) // the two propositions are just different
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: if they are already different, no separation is needed and return this       
      if (!isPairConsistent(valued)) return List(this)
      // step 4: building new bindings that respect constraints
      separatePairs(valued)
    }

  private def matchTokens(p1: Proposition, p2: Proposition): Option[List[(Token, Token)]] =
    {
      // step 0: filter verbs and length
      if (p1.verb != p2.verb || p1.length != p2.length) return None
      // match tokens into pairs
      var answer = new ListBuffer[(Token, Token)]()

      (p1.termlist, p2.termlist).zipped.foreach((_, _) match {
        case (t1: Token, t2: Token) => answer += ((t1, t2))
        case (p1: Proposition, p2: Proposition) =>
          matchTokens(p1, p2) match {
            case Some(l: List[(Token, Token)]) =>
              l.foreach { answer += _ } // recursive 
            case None => return None
          }
        case _ => return None
      })

      Some(answer.toList)
    }

  private def revealTokenValues(list: List[(Token, Token)]): List[(Token, Token)] =
    {
      list.map(elem =>
        {
          val x = elem._1
          val y = elem._2

          var t1: Token = x match {
            case v: Variable => getBoundedSymbol(v).getOrElse(v)
            case _ => x
          }

          var t2: Token = y match {
            case v: Variable => getBoundedSymbol(v).getOrElse(v)
            case _ => y
          }

          (t1, t2)
        })
    }

  /**
   * Checks if a list of token pairs is consistent. Two tokens in a "consistent" pair can be bound to the same value.
   * That is, their existing constraints do not prevent them from being the same.
   * However, they may not have been bound to the same value yet, and it is ok.
   *
   */
  private def isPairConsistent(list: List[(Token, Token)]): Boolean =
    {
      list.forall {
        x =>
          val b =
            (x._1 == x._2) || (x match {
              case (s1: PopObject, s2: PopObject) => s1 == s2
              case (t1, t2) =>
                (hashes.get(t1), hashes.get(t2)) match {
                  case (Some(vs1: VarSet), Some(vs2: VarSet)) => vs1.isCompatibleWith(vs2)
                  case _ => true
                }
            })

          debug("comparing pair for consistency: " + x + ": " + b)
          b
      }
    }

  def unifyWithConstraints(list: List[(Token, Token)], constraints: List[Proposition], initial: List[Proposition]): Option[Binding] =
    {
      var bind: Binding = this
      list.foreach {
        _ match {

          case (s1: PopObject, s2: PopObject) =>
          case (v1: Token, v2: Token) =>
            {
              (hashes.get(v1), hashes.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) =>
                  debug("unifying: " + vs1 + " " + vs2)
                  bind += vs1.mergeWith(vs2)
                case (Some(vs1: VarSet), None) =>
                  debug("unifying: " + vs1 + " " + v2)
                  bind += vs1.bindTo(v2)
                case (None, Some(vs2: VarSet)) =>
                  debug("unifying: " + v1 + " " + vs2)
                  bind += vs2.bindTo(v1)
                case (None, None) =>
                  debug("new varset for: " + v1 + " " + v2)
                  bind += VarSet(v1, v2)
              }
            }
        }
      }
      // check with initials
      val substProps = constraints map { bind substVars _ }
      debug("all constraints: " + constraints.mkString("\n"))
      debug(substProps.mkString("substed props:", ", ", "--end"))
      if (substProps.forall { p: Proposition =>
        initial exists {
          x =>
            debug {
              if (x equalsIgnoreVars p) "equaled: substed: " + substVars(x) + "\n" + substVars(p)
              else ""
            }
            x equalsIgnoreVars p
        }
      } || substProps == Nil)
        Some(bind)
      else None
    }

  private def separatePairs(list: List[(Token, Token)]): List[Binding] =
    {
      //for each one of the token pairs
      // if any is bound to a symbol, find if there is anything that would respect the constraints but not equal
      // if both are not grounded, just make them different
      // if both are symbols, ignore this pair
      var answer = ListBuffer[Binding]()
      var bind: Binding = this
      list.foreach {
        _ match {
          case (s1: PopObject, s2: PopObject) => // ignore this pair. Whether they are the same or not, we cannot do anything 
          case (v1: Token, v2: Token) =>
            {
              (hashes.get(v1), hashes.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) =>

                  if ((!vs1.isGrounded() || !vs2.isGrounded())
                    // if they are both grounded, we already know if their symbols are equal or not:
                    // if they are equal, they cannot be separated. Nothing we can do.
                    // if they are not equal, we should have already returned the current binding (i.e. "this")
                    && (!vs1.equals.exists { vs2.equals.contains(_) }) // an overlap in equals means they are really the same, so nothing we can do  
                    ) {
                    /* We do not check if v2 can still take any legal values other than vs1 and vs2's non-equals 
                     * to make the constraints work 
                     * if there is no such value, planning will fail eventually.
                     * this may be INEFFICIENT */
                    val bind = this + (vs1.bindNotTo(vs2.equalsAndSymbol), vs2.bindNotTo(vs1.equalsAndSymbol))
                    answer += bind
                  }
                case (Some(vs1: VarSet), None) =>
                  // when one variable is completely free
                  // we can always make it different from any other variable
                  val bind = this + (vs1.bindNotTo(v2), VarSet(v2).bindNotTo(vs1.equalsAndSymbol))
                  answer += bind
                case (None, Some(vs2: VarSet)) =>
                  val bind = this + (vs2.bindNotTo(v1), VarSet(v1).bindNotTo(vs2.equalsAndSymbol))
                  answer += bind
                case (None, None) =>
                  // simple case there. We can simple create two varsets and let them be different
                  val bind = this + (VarSet(v1).bindNotTo(v2), VarSet(v2).bindNotTo(v1))
                  answer += bind
              }
            }
          //          case (v1:Variable, s2:PopObject) =>
          //          case (s1:PopObject, v2:Variable) =>
        }
      }
      answer.toList
    }

  override def clone() = new Binding(hashes.clone())

  def getBoundedSymbol(v: Variable): Option[PopObject] =
    hashes.get(v) filter { _.isGrounded() } map { _.groundObj }

  def substVars(p: Proposition): Proposition =
    {
      Proposition(p.verb, p.termlist.map(_ match {
        case v: Variable =>
          getBoundedSymbol(v) getOrElse (v)

        case t: PopObject => t
        case p1: Proposition => substVars(p1)
        case other => other
      }))
    }

  def substVars(a: Action): String =
    {
      "(" + a.name + a.parameters.map { x => getBoundedSymbol(x).getOrElse(x) }.mkString(" ", " ", ")")
    }

  def substVars(p: Proposition, va: Variable, sa: PopObject): Proposition =
    {
      Proposition(p.verb, p.termlist.map(_ match {
        case v: Variable =>
          getBoundedSymbol(v) getOrElse { if (v == va) sa else v }

        case t: PopObject => t
        case p1: Proposition => substVars(p1)
        case other => other
      }))
    }

  /**
   * tests if two propositions may be equal after considering current bound variables in them
   *
   */
  def canEqual(p1: Proposition, p2: Proposition) =
    substVars(p1) equalsIgnoreVars substVars(p2)
}

object Binding {
  def apply() = new Binding()
}