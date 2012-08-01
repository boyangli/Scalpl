package parsing

import scala.util.parsing.combinator._
import scala.collection.mutable.HashMap
import scala.collection.mutable.ListBuffer

import variable._
import planning._
import action._
import structures._

object TotalParser extends AbstractPlanParser {

  def decompParse(problemFile: String, actionFile: String, recipeFile: String): (Problem, List[DecompAction], List[Recipe]) =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      var actionList = DecompActionParser.readFile(actionFile)

      //      problem.init foreach { collectObjTypes(_) }
      //      problem.goal foreach { collectObjTypes(_) }

      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }

      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }

      problem = new Problem(init, goal, problem.ontology)
      actionList = actionList map { appendTypesToTemplate(_).asInstanceOf[DecompAction] }
      actionList foreach { _.testValid() }

      var rawRecipes = DecompParser.readFile(recipeFile, actionList)
      val recipes = extractRecipes(rawRecipes, actionList.map { _.asInstanceOf[DecompAction] }, problem)

      (problem, actionList, recipes)
    }

  /**
   * We disallow a decomposition recipe to contain free variables. That is, all variables used in the recipe must be specified
   * on the higher-level action's parameter list. As our planning algorithm binds variables lazily, this does not affect the branching factor
   * or the computational complexity. This actually makes the implementation of this method easier.
   */
  protected def extractRecipes(raw: List[RawRecipe], actions: List[DecompAction], problem: Problem): List[Recipe] = {
    raw map { r =>

      val parent = findAction(Symbol(r.name), actions)

      // find the corresponding action templates, and 
      // modify the templates to use the same variables as in the recipe
      val steps = r.steps map {
        step =>
          val name = step._2.verb
          val act = findAction(name, actions)

          var arguments = ListBuffer[Variable]()
          var constraints = act.constraints
          var preconds = act.preconditions
          var effects = act.effects

          // check for type consistency
          if (step._2.termlist.size != act.parameters.size)
            throw new PopParsingException("parameter list does not match for action " + step._2.verb)
          val pairs = act.parameters zip (step._2.termlist)
          
          pairs foreach {
            
            case (standard, specified: Variable) =>
              // try to get a type from the parent action
              val typeOpt = parent.parameters.find(_.name == specified.name)
              if (typeOpt.isEmpty) {
                /* this variable is not used in the parent action. 
                 * This is disallowed. Throw an exception
                 * 
                 */
                throw new PopParsingException("Warning: " + specified + " is a free variable in decomposition.")
              } else {
                var newVar: Variable = null
                val specifiedType = typeOpt.get.pType
                if (specifiedType != standard.pType)  
                {
                  // type mismatch. check if subtype relation exists
                  if (!problem.ontology.isSubtype(specifiedType, standard.pType)) {
                    // no subtype relation exists
                    throw new PopParsingException("Action " + name + " requires type: " + standard.pType + ", but variable " +
                      specified.name + " is of type " + specifiedType + " in decomposition " + r.name)
                  } else {
                    // the specified type is a subtype of the standard type from the action
                    newVar = new Variable(specified.name, specifiedType)
                    arguments += newVar
                  }
                } else {
                  newVar = new Variable(specified.name, standard.pType)
                  arguments += newVar
                }
                // replace the standard variable with the newly made variable
                if (newVar != null && standard != newVar) {
                  constraints = constraints map { _.substitute(standard, newVar) }
                  preconds = preconds map { _.substitute(standard, newVar) }
                  effects = effects map { _.substitute(standard, newVar) }
                }
              }

            case (a, b) => // reaching this case means the second parameter is not a variable
              throw new PopParsingException("this is not a parameter: " + b)
          }

          // now this step has passed the type check
          // substituting the variables in the action template with variables in the parent

          val newAct = DecompAction(act.name, arguments.toList, constraints, preconds, effects, act.composite)

          (step._1, newAct)
      }

      // TODO: Compute causal links

      val links = r.links map {
        link =>
          val num1 = steps.indexWhere(_._1 == link._1)
          val num2 = steps.indexWhere(_._1 == link._2)

          var cond = link._3
          for (v <- cond.allVariables()) {
            cond = cond.substitute(v, parent.parameters.find(_.name == v.name).get)
          }

          new Link(num1, num2, cond, cond)
      }

      // TODO: Compute orderings
      var orderings = r.ordering map {
        order =>
          val num1 = steps.indexWhere(_._1 == order._1)
          val num2 = steps.indexWhere(_._1 == order._2)
          (num1, num2)
      }
      
      orderings = (links.map{l => (l.id1, l.id2)} ::: orderings).distinct

      new Recipe(r.name, steps map { _._2 }, links, orderings)
    }
  }

  protected def findAction(name: Symbol, actions: List[DecompAction]): DecompAction =
    {
      val actOption = actions.find(a => Symbol(a.name) == name)
      if (actOption.isEmpty) throw new PopParsingException("Cannot find action " + name + " from decomposition.")
      actOption.get
    }

  def parse(problemFile: String, actionFile: String): (Problem, List[Action]) =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      var listAction = ActionParser.readFile(actionFile)

      //      problem.init foreach { collectObjTypes(_) }
      //      problem.goal foreach { collectObjTypes(_) }

      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }

      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }

      problem = new Problem(init, goal, problem.ontology)
      listAction = listAction map { appendTypesToTemplate(_) }
      listAction foreach { _.testValid() }

      (problem, listAction)
    }

  def parseProblem(problemFile: String): Problem =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }
      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }
      new Problem(init, goal, problem.ontology)
    }

  /**
   * collects all specified object types from propositions in the problem specifications
   *
   */
  //  def collectObjTypes(prop: Proposition) {
  //    prop.termlist.foreach {
  //      _ match {
  //        case o: PopObject => if (o.pType != "Any") {
  //          typeHash.get(o.name) foreach
  //            { storedType =>
  //              if (storedType != o.pType) throw new PopParsingException("Conflicting types for object: " + o.name + " has type " + storedType + " and " + o.pType)
  //            }
  //          typeHash += (o.name -> o.pType)
  //        }
  //        case p: Proposition => collectObjTypes(p)
  //        case v: Variable => throw new PopParsingException("There should not be variables in problem specifications")
  //      }
  //    }
  //  }
}