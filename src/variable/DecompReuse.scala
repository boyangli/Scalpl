package variable
import action._
import planning._
import structures._
import scala.collection.mutable.ListBuffer

object DecompReuse {
  
  //TODO: can this be modelled with decontinuation?

  def unifyAction(action: Action, kid: Action, parent: Action, binding: Binding, plan: Plan, g: GlobalInfo): Option[Binding] = {

    // step1: match variables
    val pairs = matchVariables(action, kid, g)
    if (pairs.isEmpty) return None
    // step 2: retrieve known values for variables that are already bound
    val valued = binding.revealTokenValues(pairs.get)
    // step 3: check for superficial inconsistencies. If there is, return false
    val cons = binding.isPairConsistent(valued, g)
    if (!cons) return None
    else {
      val constraints = (action.constraints ::: kid.constraints ::: parent.constraints) filterNot { _.verb == 'neq }
      val init = plan.initialState()
      binding.unifyWithConstraints(valued, constraints, init, g.ontology)
    }
  }

  def directUnifyAction(action: Action, kid: Action, parent: Action, binding: Binding, plan: Plan, g: GlobalInfo): Option[Binding] =
    {
      val pairs = matchVariables(action, kid, g)
      val valued = binding.revealTokenValues(pairs.get)
      //println("valued = " + valued)
      val constraints = (action.constraints ::: kid.constraints ::: parent.constraints) filterNot { _.verb == 'neq }
      val init = plan.initialState()      
      binding.unifyWithConstraints(valued, constraints, init, g.ontology)
    }

  def canUnifyAction(action: Action, kid: Action, parent: Action, plan: Plan, g: GlobalInfo): Boolean = {
    //println("actions to unify: " + action + " " + kid)
    val binding = plan.binding
    // step1: match variables
    val pairs = matchVariables(action, kid, g)
    if (pairs.isEmpty) return false
    // step 2: retrieve known values for variables that are already bound
    val valued = binding.revealTokenValues(pairs.get)
    // step 3: check for superficial inconsistencies. If there is, return false
   // println("\n\nvalued: " + valued)
    val cons = binding.isPairConsistent(valued, g)
    if (!cons) return false
    else true
  }

  private def matchVariables(a1: Action, a2: Action, g: GlobalInfo): Option[List[(Variable, Variable)]] =
    {
      // step 0: filter verbs and length
      if (a1.name != a2.name || a1.parameters.length != a2.parameters.length) return None
      // match tokens into pairs
      var answer = new ListBuffer[(Variable, Variable)]()

      val ans = a1.parameters zip a2.parameters
      val pass = ans.forall {
        case (v1: Variable, v2: Variable) =>
          g.ontology.compatible(v1.pType, v2.pType) // check types of the tokens before matching them                     
      }

      if (pass) Some(ans)
      else None
    }
}