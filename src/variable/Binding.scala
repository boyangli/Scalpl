package variable

import logging._
import planning._
import structures._
import action._

import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.continuations._;

/**
 * A Binding object is a layer added on a hash map, which indexes the VarSet objects with the PopObject or Variable.
 *  It provides multiple use methods for adding VarSets into the hash map, retrieving them, etc. Most importantly,
 *  it provides the function to create a new binding which makes two propositions identical or certainly different.
 */
class Binding private (val token2varset: HashMap[Token, VarSet]) extends Logging {

  def this() = this(new HashMap[Token, VarSet]())

  def +(vs: VarSet): Binding = {
    // clone the current hash map, for the hash map is mutable.
    val bind = new Binding(token2varset.clone())
    // insert the varset and its indices into the new hash map
    vs.equals.foreach(v => bind.token2varset += (v -> vs))
    if (vs.isGrounded())
      bind.token2varset += (vs.groundObj -> vs)
    bind
  }

  def +(vss: VarSet*): Binding = {
    // here  vss is a liist of varsets
    val bind = new Binding(token2varset.clone())

    vss foreach { vs =>
      vs.equals.foreach(v => bind.token2varset += (v -> vs))
      if (vs.isGrounded())
        bind.token2varset += (vs.groundObj -> vs)
    }
    bind
  }

  /**
   * returns the varset associated with this token
   *
   */
  def get(token: Token) = token2varset.get(token)

  override def toString(): String =
    {
      token2varset.values.mkString("\n")
    }

  /**
   * attempts to build a consistent variable binding that 1) make p1 and p2 equivalent,
   * and 2) respects the given constraints.
   *
   */
  def unify(p1: Proposition, p2: Proposition, constraints: List[Proposition], initial: List[Proposition], g: GlobalInfo): Option[Binding] =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2, g)
      if (matched isEmpty) return None // fail to match the two propositions
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: check for superficial inconsistencies. If there is, return None
      if (!isPairConsistent(valued, g)) return None
      // step 4: check for constraints and, if the check passes, create a new binding that unifies the two propositions
      unifyWithConstraints(valued, constraints, initial, g.ontology)
    }

  /**
   * superficially tests for unification without checking for constraints
   * This provides a early failure that can be used as a first level filtering
   */
  def canUnify(p1: Proposition, p2: Proposition, g: GlobalInfo): Boolean =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2, g)
      if (matched isEmpty) return false // fail to match the two propositions
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: check for superficial inconsistencies. If there is, return None
      if (!isPairConsistent(valued, g)) return false
      else return true
    }

  var unifyCont: Unit => Option[Binding] = null
  /**
   * superficially tests for unification without checking for constraints
   * This provides a early failure that can be used as a first level filtering
   * This is the first part of the continuation version of unification
   */
  def canUnifyPart1(p1: Proposition, p2: Proposition, constraints: () => List[Proposition], initial: List[Proposition], g: GlobalInfo): Boolean =
    {
      reset {
        // step 1: match tokens pairwise
        val matched = matchTokens(p1, p2, g)
        if (matched isEmpty) return false // fail to match the two propositions
        // step 2: retrieve known values for variables that are already bound
        val valued = revealTokenValues(matched.get)
        // step 3: check for superficial inconsistencies. If there is, return None
        if (!isPairConsistent(valued, g)) return false

        shift {
          k: (Unit => Option[Binding]) =>
            {
              unifyCont = k
              true
            }
        }
        unifyWithConstraints(valued, constraints(), initial, g.ontology)
      }
    }

  def continueUnify() =
    {
      if (unifyCont == null) throw new BindingException("invalid continuation")
      else {
        unifyCont()
      }
    }

  /**
   * directly unify without testing for superficial inconsistency (steps 1-3 in the {@code canUnify()} method)
   * still tests for consistency with constraints
   *
   */
  def directUnify(p1: Proposition, p2: Proposition, constraints: List[Proposition], initial: List[Proposition], g: GlobalInfo): Option[Binding] =
    {
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2, g)
      val valued = revealTokenValues(matched.get)
      unifyWithConstraints(valued, constraints, initial, g.ontology)
    }

  /**
   * add a bunch of neq propositions into the binding
   *
   */
  def addNeqs(neqs: List[Proposition], ontology: Ontology): Binding =
    {
      var newbind = this
      neqs foreach { neq =>
        // iteam 1 and item2 are the two parameters of the neq proposition
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
  
  /** This is another implementation of addNeqs. From the code, it seems to be faster because it saves the
   * expensive copy operations. In reality, it is actually slower. I don't understand why.
   */
  private def addNeqsSlower(neqs: List[Proposition], ontology: Ontology): Binding =
    {

      var newMap = token2varset.clone
      neqs foreach { neq =>
        // item 1 and item2 are the two parameters of the neq proposition
        val item1 = neq.termlist(0).asInstanceOf[Token]
        val item2 = neq.termlist(1).asInstanceOf[Token]

        (newMap.get(item1), newMap.get(item2)) match {
          case (Some(vs1: VarSet), Some(vs2: VarSet)) =>

            val newvs1 = vs1.bindNotTo(vs2.equals)
            val newvs2 = vs2.bindNotTo(vs1.equals)
            // update the mapping between variables and varsets
            newvs1.equals.foreach(v => newMap += (v -> newvs1))
            if (newvs1.isGrounded())
              newMap += (newvs1.groundObj -> newvs1)

            newvs2.equals.foreach(v => newMap += (v -> newvs2))
            if (newvs2.isGrounded())
              newMap += (newvs2.groundObj -> newvs2)

          case (Some(vs1: VarSet), None) =>
            val newvs1 = vs1.bindNotTo(item2)
            val newvs2 = VarSet(item2).bindNotTo(item1)

            newvs1.equals.foreach(v => newMap += (v -> newvs1))
            if (newvs1.isGrounded())
              newMap += (newvs1.groundObj -> newvs1)

            newvs2.equals.foreach(v => newMap += (v -> newvs2))
            if (newvs2.isGrounded())
              newMap += (newvs2.groundObj -> newvs2)

          case (None, Some(vs2: VarSet)) =>
            val newvs1 = VarSet(item1).bindNotTo(item2)
            val newvs2 = vs2.bindNotTo(item1)

            newvs1.equals.foreach(v => newMap += (v -> newvs1))
            if (newvs1.isGrounded())
              newMap += (newvs1.groundObj -> newvs1)

            newvs2.equals.foreach(v => newMap += (v -> newvs2))
            if (newvs2.isGrounded())
              newMap += (newvs2.groundObj -> newvs2)

          case (None, None) =>
            val newvs1 = VarSet(item1).bindNotTo(item2)
            val newvs2 = VarSet(item2).bindNotTo(item1)

            newvs1.equals.foreach(v => newMap += (v -> newvs1))
            if (newvs1.isGrounded())
              newMap += (newvs1.groundObj -> newvs1)

            newvs2.equals.foreach(v => newMap += (v -> newvs2))
            if (newvs2.isGrounded())
              newMap += (newvs2.groundObj -> newvs2)

        }
      }
      
      new Binding(newMap)
    }

  /**
   * attempts to build a set of consistent variable binding that 1) make p1 and p2 different,
   * and 2) respects the given constraints.
   *
   */
  def separate(p1: Proposition, p2: Proposition, g: GlobalInfo): List[Binding] =
    {
      // step 0: if they are already the same, no separation is possible and return Nil
      if (substVars(p1) == substVars(p2)) return Nil
      // step 1: match tokens pairwise
      val matched = matchTokens(p1, p2, g)
      if (matched isEmpty) return List(this) // the two propositions are just different, no further operations necessary
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: if they are already different, no separation is needed and return this       
      if (!isPairConsistent(valued, g)) return List(this)
      // step 4: building new bindings that respect constraints
      separatePairs(valued)
    }

  /**
   * match tokens in two propositions into a list of pairs. If it returns None,
   * matching has failed due to their inherent differences
   */
  private def matchTokens(p1: Proposition, p2: Proposition, g: GlobalInfo): Option[List[(Token, Token)]] =
    {
      // step 0: filter verbs and length
      if (p1.verb != p2.verb || p1.length != p2.length) return None
      // match tokens into pairs
      var answer = new ListBuffer[(Token, Token)]()

      (p1.termlist, p2.termlist).zipped foreach {
        (_, _) match {
          case (v1: Variable, v2: Variable) => {
            if (typeCompatible(v1.pType, v2.pType, g)) // check types of the tokens before matching them
              answer += ((v1, v2))
            else return None
          }
          case (v1: Variable, o2: PopObject) => {
            if (o2.pType == v1.pType || g.ontology.isSubtype(o2.pType, v1.pType))
              answer += ((v1, o2))
            else return None
          }
          case (o1: PopObject, v2: Variable) => {
            if (o1.pType == v2.pType || g.ontology.isSubtype(o1.pType, v2.pType))
              answer += ((o1, v2))
            else return None
          }
          case (o1: PopObject, o2: PopObject) => {
            if (o1 == o2)
              answer += ((o1, o2))
            else return None
          }
          case (p1: Proposition, p2: Proposition) =>
            matchTokens(p1, p2, g) match {
              case Some(l: List[(Token, Token)]) =>
                l.foreach { answer += _ } // recursive 
              case None => return None
            }
          case _ => return None
        }
      }

      Some(answer.toList)
    }

  private def looseMatchTokens(p1: Proposition, p2: Proposition, g: GlobalInfo): Option[List[(Token, Token)]] =
    {
      // step 0: filter verbs and length
      if (p1.verb != p2.verb || p1.length != p2.length) return None
      // match tokens into pairs
      var answer = new ListBuffer[(Token, Token)]()

      (p1.termlist, p2.termlist).zipped.foreach((_, _) match {
        case (t1: Token, t2: Token) => {
          // check types of the tokens before matching them
          // in the loose match, it is ok if the ptypes do not match as long as none of them is person
          if ((t1.pType != "Person" && t2.pType != "Person") || typeCompatible(t1.pType, t2.pType, g))
            answer += ((t1, t2))
          else return None
        }
        case (p1: Proposition, p2: Proposition) =>
          matchTokens(p1, p2, g) match {
            case Some(l: List[(Token, Token)]) =>
              l.foreach { answer += _ } // recursive 
            case None => return None
          }
        case _ => return None
      })

      Some(answer.toList)
    }

  /**
   * replace variables with the symbols they are bounded with
   *
   */
  private[variable] def revealTokenValues(list: List[(Token, Token)]): List[(Token, Token)] =
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
  private[variable] def isPairConsistent(list: List[(Token, Token)], g: GlobalInfo): Boolean =
    {
      implicit val ontology = g.ontology
      list.forall {
        x =>
          val b =
            (x._1 == x._2) || (x match {
              // two objects are only compatible if they are equal
              case (s1: PopObject, s2: PopObject) => s1 == s2
              // if one of the two is a variable,
              // they are compatible as long as their varsets are
              case (t1, t2) =>
                (token2varset.get(t1), token2varset.get(t2)) match {
                  case (Some(vs1: VarSet), Some(vs2: VarSet)) =>
                    //println("vs1 = " + vs1 + " vs2 = " + vs2)
                    debug { "vs1 = " + vs1 + " vs2 = " + vs2 }
                    vs1.isCompatibleWith(vs2) && matchTokenTypes(t1, vs1.pType, t2, vs2.pType, ontology)
                  case (Some(vs1: VarSet), None) => matchTokenTypes(t1, vs1.pType, t2, t2.pType, ontology)
                  // TODO: this is not correct!!!!!!
                  case (None, Some(vs2: VarSet)) => matchTokenTypes(t1, t1.pType, t2, vs2.pType, ontology)
                  case (None, None) => matchTokenTypes(t1, t1.pType, t2, t2.pType, ontology)
                }
            })
          //println("comparing pair for consistency: " + x + ": " + b)
          debug("comparing pair for consistency: " + x + ": " + b)
          b
      }
    }

  private def matchTokenTypes(t1: Token, t1cap: String, t2: Token, t2cap: String, ontology: Ontology): Boolean =
    {
      (t1, t2) match {
        case (v1: Variable, v2: Variable) => ontology.compatible(t1cap, t2cap)
        case (o1: PopObject, v2: Variable) => t1cap == t2cap || ontology.isSubtype(t1cap, t2cap)
        case (v1: Variable, o2: PopObject) => t1cap == t2cap || ontology.isSubtype(t2cap, t1cap)
        case (o1: PopObject, o2: PopObject) => t1cap == t2cap
      }
    }

  def unifyWithConstraints(list: List[(Token, Token)], constraints: List[Proposition], initial: List[Proposition], ontology: Ontology): Option[Binding] =
    {
      implicit val on = ontology
      var bind: Binding = this
      list.foreach {
        _ match {

          case (s1: PopObject, s2: PopObject) => // their consistency is already checked, so we don't check again
          case (v1: Token, v2: Token) =>
            {
              (token2varset.get(v1), token2varset.get(v2)) match {
                case (Some(vs1: VarSet), Some(vs2: VarSet)) =>
                  debug("unifying: " + vs1 + " " + vs2)

                  // varset compatibility
                  if (!ontology.compatible(vs1.pType, vs2.pType)) return None

                  bind += vs1.mergeWith(vs2)
                case (Some(vs1: VarSet), None) =>
                  debug("unifying: " + vs1 + " " + v2)

                  // varset compatibility
                  if (!ontology.compatible(vs1.pType, v2.pType)) return None

                  bind += vs1.bindTo(v2)
                case (None, Some(vs2: VarSet)) =>
                  debug("unifying: " + v1 + " " + vs2)

                  // varset compatibility
                  if (!ontology.compatible(v1.pType, vs2.pType)) return None

                  bind += vs2.bindTo(v1)
                case (None, None) =>
                  debug("new varset for: " + v1 + " " + v2)

                  // varset compatibility
                  if (!ontology.compatible(v1.pType, v2.pType)) return None

                  bind += VarSet(v1, v2)
              }
            }
        }
      }
      // check constraints
      if (constraints == Nil) return Some(bind)
      // substitute bound variables
      val substProps = constraints map { bind substVars _ }
      debug(constraints.mkString("all constraints: ", ", ", " --end"))
      debug(substProps.mkString("substed props:", ", ", " --end"))
      // check the constraints for their existence in the initial state
      if (substProps.forall { p: Proposition =>
        initial exists {
          x =>
            debug {
              if (x equalsIgnoreVars p) "equaled: substed: " + substVars(x) + "\n" + substVars(p)
              else ""
            }
            x equalsIgnoreVars p
        }
      })
        Some(bind)
      else None
    }

  private def separatePairs(list: List[(Token, Token)]): List[Binding] =
    {
      //for each one of the token pairs
      // if any is bound to an object, (find if there is anything that would respect the constraints but not equal) (not done)
      // if both are not grounded, just make them different
      // if both are objects, ignore this pair
      var answer = ListBuffer[Binding]()
      var bind: Binding = this
      list.foreach {
        _ match {
          case (s1: PopObject, s2: PopObject) => // ignore this pair. Whether they are the same or not, we cannot do anything 
          case (v1: Token, v2: Token) =>
            {
              (token2varset.get(v1), token2varset.get(v2)) match {
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
                     * this may be INEFFICIENT, but correct */
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
                  // simple case here. We can simple create two varsets and let them be different
                  val bind = this + (VarSet(v1).bindNotTo(v2), VarSet(v2).bindNotTo(v1))
                  answer += bind
              }
            }
        }
      }
      answer.toList
    }

  override def clone() = new Binding(token2varset.clone())

  def getBoundedSymbol(v: Variable): Option[PopObject] =
    token2varset.get(v) filter { _.isGrounded() } map { _.groundObj }

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

  def substVarsString(a: Action): String =
    {
      "(" + a.name + a.parameters.map { x => getBoundedSymbol(x).getOrElse(x) }.mkString(" ", " ", ")")
    }

  def substVarsShortString(a: Action): String =
    {
      "(" + a.name + a.parameters.map { x => getBoundedSymbol(x).getOrElse(x).toShortString }.mkString(" ", " ", ")")
    }

  /*
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
    */

  /**
   * tests if two propositions may be equal after considering current bound variables in them
   *
   */
  def canEqual(p1: Proposition, p2: Proposition) =
    substVars(p1) equalsIgnoreVars substVars(p2)

  def typeCompatible(type1: String, type2: String, g: GlobalInfo): Boolean =
    {
      if (type1 == type2) return true
      else {
        val ontology = g.ontology
        if (ontology == null) return false

        // subclass1 and 2 are sets of subclasses of their types
        val subclass1 = ontology.subTypes(type1)
        val subclass2 = ontology.subTypes(type2)

        if (subclass1.isEmpty && subclass2.isEmpty) false
        else if (subclass1.isEmpty && subclass2.isDefined) (subclass2.get contains type1)
        else if (subclass1.isDefined && subclass2.isEmpty) (subclass1.get contains type2)
        else (subclass1.get contains type2) || (subclass2.get contains type1)
      }
    }

  def analogicallyUnify(p1: Proposition, p2: Proposition, constraints: List[Proposition],
    initial: List[Proposition], g: GadgetGlobal): Option[Binding] =
    {
      // step 1: match tokens pairwise
      val matched = looseMatchTokens(p1, p2, g)
      if (matched isEmpty) return None // fail to match the two propositions
      // step 2: retrieve known values for variables that are already bound
      val valued = revealTokenValues(matched.get)
      // step 3: check for superficial inconsistencies. If there is, return None
      if (!isPairConsistent(valued, g)) return None
      unifyWithConstraints(valued, constraints, initial, g.ontology)
    }
}

object Binding {
  def apply() = new Binding()
}