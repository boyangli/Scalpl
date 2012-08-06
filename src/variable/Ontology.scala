package variable

/**
 * A simple implementation of an ontology.
 * Making it a separate object allows future refactoring.
 *
 */
class Ontology(protected var subclasses: Map[String, Set[String]]) {

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

  /** returns if two types are compatible
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

  def mergeWith(that: Ontology): Ontology =
    {
      // handle situations involving nulls
      if (that == null) return this

      var answer = subclasses
      // Map is immutable. We can directly modify it without affecting this instance
      val other = that.subclasses
      // combining answers
      other.keys foreach {
        key =>
          val content2 = other(key)
          answer.get(key) match {
            case Some(content1) => answer += ((key, content1 ++ content2))
            case None => answer += ((key, content2))
          }
      }

      val o = new Ontology(answer)
      o.refresh()
      o
    }

  protected def refresh() {
    var another = subclasses
    subclasses.keys foreach { key =>
      val content = subclasses(key)
      content foreach { subtype =>
        subclasses.get(subtype) match {
          case Some(content1) => another += ((key, content ++ content1))
          case None =>
        }
      }
    }

    subclasses = another
  }

  def subTypes(pType: String) = subclasses.get(pType)
  def subTypesOrEmpty(pType: String) = subclasses.getOrElse(pType, Set())
  
  override def toString() = "Ontology: " + subclasses.toString
}

object Ontology {
  def emptyInstance() = new Ontology(Map[String, Set[String]]())
}