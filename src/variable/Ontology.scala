package variable
import action._
import parsing.PopParsingException
/**
 * A simple implementation of an ontology.
 * Making it a separate object allows future refactoring.
 *
 */
class Ontology(protected val subclasses: Map[String, Set[String]], // from a super class to its subclasses
  protected val objectHash: Map[String, Token] // from object name to their tokens
  ) {

  // from a type to its range of value, i.e. objects belonging to this type
  val domains: Map[String, List[Token]] = {
    subclasses.keySet map { cl: String =>

      val range = objectHash.values.filter { token =>
        token.pType == cl || subclasses(cl).contains(token.pType)
      }

      (cl, range.toList)
    } toMap
  }

  def appendTypesTo(p: Proposition): Proposition =
    {
      //println("dealing with " + p)
      val newterms =
        p.termlist.map {
          _ match {
            case o: PopObject =>
              if (o.pType == "Any")
                objectHash.get(o.name) match {
                  case Some(storedObj) => storedObj
                  case None => throw new PopParsingException("The type of object " + o.name + " cannot be inferred.")
                }
              else o
            case v: Variable =>
              if (v.pType == "Any") objectHash.get(v.fullName) match {
                case Some(storedVar) => storedVar
                case None => throw new PopParsingException("The type of variable " + v.fullName + " cannot be inferred.")
              }
              else v
            case prop: Proposition => appendTypesTo(prop)
          }
        }
      Proposition(p.verb, newterms)
    }

  def appendTypesTo(action: Action): Action =
    {
      val newConstraints = action.constraints map { appendTypesTo(_) }
      val newPreconds = action.preconditions map { appendTypesTo(_) }
      val newEffects = action.effects map { appendTypesTo(_) }
      Action(action.id, action.name, action.parameters, newConstraints, newPreconds, newEffects)
    }

  def appendTypesToTemplate(action: Action): Action =
    {
      if (action.parameters.exists(v => v.pType == "Any")) throw new PopParsingException("unspecified parameter type in action: " + action)

      var actionHash = scala.collection.mutable.HashMap() ++ objectHash
      action.parameters map { v =>
        if (actionHash.get(v.name).isDefined) throw new PopParsingException("name conflicts: " + v.name + " is repeated.")
        else
          actionHash += (v.name -> v)
        // note here we use the name field, not the full name field. 
        // As the templates are not instantiated, the two fields are actually identical
      }

      val newConstraints = action.constraints map { appendTypesTo(_, actionHash) }

      val newPreconds = action.preconditions map { appendTypesTo(_, actionHash) }
      val newEffects = action.effects map { appendTypesTo(_, actionHash) }
      val actor = action.actor match {
        case v: Variable => if (v.pType == "Any") getOrError(v, v.name, actionHash) else v
        case o: PopObject => if (o != PopObject.unknown && o.pType == "Any") getOrError(o, o.name, actionHash) else o
      }

      // must handle DecompAction here
      action match {
        case dact: DecompAction =>
          DecompAction(action.name, actor, action.parameters, newConstraints, newPreconds, newEffects, dact.composite)
        case _ =>
          Action(action.name, actor, action.parameters, newConstraints, newPreconds, newEffects)
      }
    }

  /**
   * infers types for objects and variables from the hashmap
   *
   */
  protected def appendTypesTo(p: Proposition, hashmap: scala.collection.mutable.Map[String, Token]): Proposition =
    {
      val newtermlist = p.termlist map
        {
          _ match {
            case o: PopObject =>
              if (o.pType == "Any")
                getOrError(o, o.name, hashmap)
              else o
            case p: Proposition => appendTypesTo(p, hashmap)
            case v: Variable =>
              if (v.pType == "Any")
                getOrError(v, v.name, hashmap)
              else v
          }
        }
      new Proposition(p.verb, newtermlist)
    }

  protected def getOrError(token: Token, name: String, hash: scala.collection.mutable.Map[String, Token]): Token =
    {
      hash.get(name) match {
        case Some(stored) => stored
        case None => token match {
          case v: Variable => throw new PopParsingException("The type of variable " + v.name + " cannot be inferred.")
          case o: PopObject => throw new PopParsingException("The type of object " + o.name + " cannot be inferred.")
        }
      }
    }

  /**
   * determines if one type is a subtype of the other type
   *
   */
  def isSubtype(subType: String, superType: String): Boolean = {
    if (superType == "Any") true
    else if (subclasses.contains(superType))
      subclasses(superType).contains(subType)
    else false
  }

  /**
   * returns the subtype of the two types. If none belongs to the other, returns None
   *
   */
  def lower(type1: String, type2: String): Option[String] = {
    if (type1 == type2) return Some(type1)
    else if (type1 == "Any") return Some(type2)
    else if (type2 == "Any") return Some(type1)
    else if ((!subclasses.contains(type1)) && (!subclasses.contains(type2)))
      return None
    else if ((subclasses contains type1) && (subclasses(type1) contains type2))
      return Some(type2)
    else if ((subclasses contains type2) && (subclasses(type2) contains type1))
      return Some(type1)
    else return None
  }

  /**
   * returns if two types are compatible
   *
   */
  def compatible(type1: String, type2: String): Boolean = {
    if (type1 == type2) true
    else if (type1 == "Any" || type2 == "Any") true
    else if ((!subclasses.contains(type1)) && (!subclasses.contains(type2)))
      false
    else if ((subclasses contains type1) && (subclasses(type1) contains type2))
      true
    else if ((subclasses contains type2) && (subclasses(type2) contains type1))
      true
    else false
  }

  //  def mergeWith(that: Ontology): Ontology =
  //    {
  //      // handle situations involving nulls
  //      if (that == null) return this
  //
  //      var answer = subclasses
  //      // Map is immutable. We can directly modify it without affecting this instance
  //      val other = that.subclasses
  //      // combining answers
  //      other.keys foreach {
  //        key =>
  //          val content2 = other(key)
  //          answer.get(key) match {
  //            case Some(content1) => answer += ((key, content1 ++ content2))
  //            case None => answer += ((key, content2))
  //          }
  //      }
  //
  //      val o = new Ontology(answer)
  //      o.refresh()
  //      o
  //    }

  //  protected def refresh() {
  //    var another = subclasses
  //    subclasses.keys foreach { key =>
  //      val content = subclasses(key)
  //      content foreach { subtype =>
  //        subclasses.get(subtype) match {
  //          case Some(content1) => another += ((key, content ++ content1))
  //          case None =>
  //        }
  //      }
  //    }
  //
  //    subclasses = another
  //  }

  def subTypes(pType: String) = subclasses.get(pType)
  def subTypesOrEmpty(pType: String) = subclasses.getOrElse(pType, Set())

  override def toString() = "Ontology: " + subclasses.toString

  // TODO: Find and store all objects of a particular type
  def objectsOfType(objtype: String): List[PopObject] = Nil
  def knownTypeOf(obj: String): String = "Any"
}

//object Ontology {
//  def emptyInstance() = new Ontology(Map[String, Set[String]]())
//}