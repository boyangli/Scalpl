package variable

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import PopSymbol._
import plan._

class Binding private (val hashes: HashMap[Token, VarSet]) {
  //  var varsets = List[VarSet]()
  //  val hashes = new HashMap[Variable, VarSet]()
  // 
  def this() = this(new HashMap[Token, VarSet]())

  def +(vs: VarSet): Binding = {
    val bind = new Binding(hashes.clone())
    vs.equals.foreach(v => bind.hashes += (v -> vs))
    if (vs.isGrounded())
    	bind.hashes += (vs.symbol -> vs)
    bind
  }

  def +(vss: VarSet*): Binding = {
    val bind = new Binding(hashes.clone())

    vss foreach { vs =>
      vs.equals.foreach(v => bind.hashes += (v -> vs))
      if (vs.isGrounded())
    	  bind.hashes += (vs.symbol -> vs)
    }
    bind
  }
  
  def get(token:Token) = hashes.get(token)

  override def toString(): String =
    {
      hashes.values.mkString("\n")
    }

  /**
   *  returns (?, ?) if two variables can unify. the second ? can be null, indicating no actions are needed.
   *  Otherwise, return null
   */
  def canUnify(v1: Variable, v2: Variable): (Variable, Token) =
    {
      val svs1 = hashes.get(v1)
      val svs2 = hashes.get(v2)

      (svs1, svs2) match {
        case (Some(vs1), Some(vs2)) =>
          {
            if (vs1 == vs2)
              (v1, null)
            else if (!vs1.isGrounded() && !vs2.nonEquals.contains(v1))
              (v1, v2)
            else if (!vs2.isGrounded && !vs1.nonEquals.contains(v2))
              (v2, v1)
            else null
          }
        case (Some(vs1), None) =>
          {
            if (!vs1.nonEquals.contains(v2))
              (v2, v1)
            else null
          }
        case (None, Some(vs2)) =>
          {
            if (!vs2.nonEquals.contains(v1))
              (v1, v2)
            else null
          }
        case (None, None) =>
          {
            (v1, v2)
          }
      }
    }

  //  def canUnify(token1: Token, token2: Token, constraints: List[Proposition], inits: List[Proposition]): Boolean =
  //    {
  //      (token1, token2) match {
  //        case (s1: PopSymbol, s2: PopSymbol) =>
  //          {
  //            if (s1 == s2)
  //              true
  //            else false
  //          }
  //        case (v: Variable, s: PopSymbol) =>
  //          {
  //            canUnifyVS(v, s, constraints, inits)
  //          }
  //        case (s: PopSymbol, v: Variable) =>
  //          {
  //            canUnifyVS(v, s, constraints, inits)
  //          }
  //        case (v1: Variable, v2: Variable) =>
  //          {
  //            canUnifyVV(v1, v2, constraints, inits)
  //          }
  //      }
  //    }
  //
  def canUnifyVS(v: Variable, s: Symbol, constraints: List[Proposition], inits: List[Proposition]): Boolean =
    {
      hashes.get(v) match {
        case Some(varset: VarSet) =>
          {
            if (varset.symbol == s) // already bound. trivial case
              return true
            else if (varset.nonEquals.contains(s)) // already non-equal. another trivial case
              return false
            // v has no relations with s. Hence, we cannot filter here
          }
        case None =>
      }
      // this is the more tedious test used when the relation between v and s is not obvious
      // we attempt to substitute all known variables in the constraints list
      // and then compare to the initial state
      val realCons = constraints.map(this.substVars(_, v, s))
      for (pc <- realCons) {
        // at least one condition in the initial state must equal to pc if we ignore unbound variables
        val possible = inits.exists(pi => pi.equalsIgnoreVars(pc))
        if (!possible) return false
      }
      true
    }

  // TODO: 8/21 fix this
  //  def canUnifyVV(v1: Variable, v2: Variable, constraints: List[Proposition], inits: List[Proposition]): Boolean =
  //    {
  //      // do trivial filtering here
  //      hashes.get(v) match {
  //        case Some(varset: VarSet) =>
  //          {
  //            if (varset.symbol == s) // already bound. trivial case
  //              return true
  //            else if (varset.nonEquals.contains(s)) // already non-equal. another trivial case
  //              return false
  //            // v has no relations with s. Hence, we cannot filter here
  //          }
  //        case None =>
  //      }
  //      // this is the more tedious test used when the relation between v and s is not obvious
  //      // we attempt to substitute all known variables in the constraints list
  //      // and then compare to the initial state
  //      val realCons = constraints.map(this.substVars(_, v, s))
  //      for (pc <- realCons) {
  //        // at least one condition in the initial state must equal to pc if we ignore unbound variables
  //        val possible = inits.exists(pi => pi.equalsIgnoreVars(pc))
  //        if (!possible) return false
  //      }
  //      true
  //    }
  //

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
   * attempts to build a set of consistent variable binding that 1) make p1 and p2 different,
   * and 2) respects the given constraints.
   *
   */
  def separate(p1: Proposition, p2: Proposition, constraints: List[Proposition], initial: List[Proposition]): List[Binding] =
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
          matchTokens(p1, p2) foreach { answer ++= _ } // recursive             
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

  /** Checks if a list of token pairs is consistent. Two tokens in a "consistent" pair can be bound to the same value.
   * That is, their existing constraints do not prevent them from being the same.
   * However, they may not have been bound to the same value yet, and it is ok.
   * 
   */
  private def isPairConsistent(list: List[(Token, Token)]): Boolean =
    {
      list.forall {

        /* this is *probably* not the correct behavior
          _ match {  
          case (v1: Variable, v2: Variable) => (v1 == v2) ||
            {
              (hashes.get(v1), hashes.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) => vs1.isCompatibleWith(vs2)
                case _ => true
              }
            }
          case (s1: PopSymbol, v2: Variable) =>
            //            println(hashes get(v2) forall { !_.nonEquals.contains(s1) }) 
            hashes get (v2) forall { !_.nonEquals.contains(s1) } // v2's varset does not contain s1 as non-equals
          case (v1: Variable, s2: PopSymbol) =>
            hashes get (v1) forall { !_.nonEquals.contains(s2) } // v1's varset does not contain s2 as non-equals
          case (s1: PopSymbol, s2: PopSymbol) => s1 == s2
          }
          */
        x =>          
          println("comparing pair: " + x)
          (x._1 == x._2) || (x match
          {
            case (s1:PopSymbol, s2:PopSymbol) => s1 == s2
            case (t1, t2) =>            
              (hashes.get(t1), hashes.get(t2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) => vs1.isCompatibleWith(vs2)
                case _ => true
              }
          })
      }
    }

  private def unifyWithConstraints(list: List[(Token, Token)], constraints: List[Proposition], initial: List[Proposition]): Option[Binding] =
    {
      var bind: Binding = this
      list.foreach {
        _ match {

          /* this is *propably* not the correct behavior
          case (v1: Variable, v2: Variable) =>
            {
              (hashes.get(v1), hashes.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) => bind += vs1.mergeWith(vs2)
                case (Some(vs1: VarSet), None) => bind += vs1.bindTo(v2)
                case (None, Some(vs2: VarSet)) => bind += vs2.bindTo(v1)
                case (None, None) => bind += VarSet(v1, v2)
              }
            }
          case (s1: PopSymbol, v2: Variable) =>
            bind += (hashes get (v2) map { _ bindTo (s1) } getOrElse (VarSet(s1, v2)))
          case (v1: Variable, s2: PopSymbol) =>
            bind += (hashes get (v1) map { _ bindTo (s2) } getOrElse (VarSet(s2, v1)))
          case (s1: PopSymbol, s2: PopSymbol) =>
          */

          case (s1: PopSymbol, s2: PopSymbol) =>
          case (v1: Token, v2: Token) =>
            {
              (hashes.get(v1), hashes.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) =>
                  //println("unifying: " + vs1 + " " + vs2)
                  bind += vs1.mergeWith(vs2)
                case (Some(vs1: VarSet), None) =>
                  //println("unifying: " + vs1 + " " + v2)
                  bind += vs1.bindTo(v2)
                case (None, Some(vs2: VarSet)) =>
                  //println("unifying: " + v1 + " " + vs2)
                  bind += vs2.bindTo(v1)
                case (None, None) =>
                  //println("new varset for: " + v1 + " " + v2)
                  bind += VarSet(v1, v2)
              }
            }
        }
      }
      // check with initials
      val substProps = constraints map { bind substVars _ }
      println("substed props:" + substProps.mkString("\n"))
      if (substProps forall { p => initial exists { _ equalsIgnoreVars p } })
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
          case (s1: PopSymbol, s2: PopSymbol) => // ignore this pair. Whether they are the same or not, we cannot do anything 
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
                    val bind = this + (vs1.bindNotTo(vs2.equals), vs2.bindNotTo(vs1.equals))
                    answer += bind
                  }
                case (Some(vs1: VarSet), None) =>
                  // when one variable is completely free
                  // we can always make it different from any other variable
                  val bind = this + (vs1.bindNotTo(v2), VarSet(v2).bindNotTo(vs1.equals))
                  answer += bind
                case (None, Some(vs2: VarSet)) =>
                  val bind = this + (vs2.bindNotTo(v1), VarSet(v1).bindNotTo(vs2.equals))
                  answer += bind
                case (None, None) =>
                  // simple case there. We can simple create two varsets and let them be different
                  val bind = this + (VarSet(v1).bindNotTo(v2), VarSet(v2).bindNotTo(v1))
                  answer += bind
              }
            }
//          case (v1:Variable, s2:PopSymbol) =>
//          case (s1:PopSymbol, v2:Variable) =>
        }
      }
      answer.toList
    }

  override def clone() = new Binding(hashes.clone())

  def getBoundedSymbol(v: Variable): Option[PopSymbol] =
    hashes.get(v) filter { _.isGrounded() } map { _.symbol }

  def substVars(p: Proposition): Proposition =
    {
      Proposition(p.verb, p.termlist.map(_ match {
        case v: Variable =>
          getBoundedSymbol(v) getOrElse (v)

        case t: PopSymbol => t
        case p1: Proposition => substVars(p1)
        case other => other
      }))
    }

  def substVars(a: Action): String =
    {
      "(" + a.name + a.parameters.map { x => getBoundedSymbol(x).getOrElse(x) }.mkString(" ", " ", ")")
    }

  def substVars(p: Proposition, va: Variable, sa: PopSymbol): Proposition =
    {
      Proposition(p.verb, p.termlist.map(_ match {
        case v: Variable =>
          getBoundedSymbol(v) getOrElse { if (v == va) sa else v }

        case t: PopSymbol => t
        case p1: Proposition => substVars(p1)
        case other => other
      }))
    }
}