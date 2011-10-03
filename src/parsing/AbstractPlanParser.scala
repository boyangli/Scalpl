package parsing
import scala.collection.mutable.HashMap

import variable._
import plan._

abstract class AbstractPlanParser extends PopParser {
  var objectHash = HashMap[String, Token]()

  /**
   * associate all the variables with their full names (e.g. "v-?name-1") in the hash table
   *
   */
  def storeAllVariables(actions: List[Action]) {
    objectHash ++=
      actions flatMap {
        _.parameters map {
          p => (p.fullName -> p)
        }
      }
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
      new Action(action.id, action.name, action.parameters, newConstraints, newPreconds, newEffects)
    }

  def collectTypes(prop: Proposition) {
    prop.termlist.foreach {
      _ match {
        case o: PopObject => if (o.pType != "Any") {
          objectHash.get(o.name) foreach
            { storedType =>
              if (storedType != o.pType) throw new PopParsingException("Conflicting types for object: " + o.name + " has type " + storedType + " and " + o.pType)
            }
          objectHash += (o.name -> o)
        }
        case p: Proposition => collectTypes(p)
        case v: Variable => throw new PopParsingException("There should not be variables in problem specifications")
      }
    }
  }

  def appendTypesToTemplate(action: Action): Action =
    {
      if (action.parameters.exists(v => v.pType == "Any")) throw new PopParsingException("unspecified parameter type in action: " + action)

      var actionHash = objectHash.clone()
      action.parameters foreach { v =>
        if (actionHash.get(v.name).isDefined) throw new PopParsingException("name conflicts: " + v.name + " is repeated.")
        else
          actionHash += (v.name -> v)
      }

      val newConstraints = action.constraints map { appendTypesTo(_, actionHash) }

      val newPreconds = action.preconditions map { appendTypesTo(_, actionHash) }
      val newEffects = action.effects map { appendTypesTo(_, actionHash) }

      Action(action.name, action.parameters, newConstraints, newPreconds, newEffects)
    }

  /**
   * infers types for objects and variables from the hashmap
   *
   */
  def appendTypesTo(p: Proposition, hashmap: HashMap[String, Token]): Proposition =
    {
      val newtermlist = p.termlist map
        {
          _ match {
            case o: PopObject =>
              if (o.pType == "Any")
                hashmap.get(o.name) match {
                  case Some(storedObj) => storedObj
                  case None => throw new PopParsingException("The type of object " + o.name + " cannot be inferred.")
                }
              else o
            case p: Proposition => appendTypesTo(p, hashmap)
            case v: Variable => if (v.pType == "Any")
              hashmap.get(v.name) match {
                case Some(storedVar) => storedVar
                case None => throw new PopParsingException("The type of variable " + v.name + " cannot be inferred.")
              }
            else v
          }
        }
      new Proposition(p.verb, newtermlist)
    }
}