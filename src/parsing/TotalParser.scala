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
      val problem = ProblemParser.readFile(problemFile)
      val ontology = problem.ontology
      var actionList = DecompActionParser.readFile(actionFile)

      actionList = actionList map { ontology.appendTypesToTemplate(_).asInstanceOf[DecompAction] }
      
      // test the validity of actions for once
      actionList foreach { _.testValid() }

      var rawRecipes = DecompParser.readFile(recipeFile, actionList)
      val recipes = extractRecipes(rawRecipes, actionList.map { _.asInstanceOf[DecompAction] }, problem)

      (problem, actionList, recipes)
    }

  /** Extracts the Recipe data structure from crudely parsed inputs.
   * 
   * The method has been changed to make use of variable bindings instead of variable substitutions. 
   * This results in a flexible representation, albeit a bit more complex coding, that allows the following two advantages:
   * 
   * To make authoring easier, we allow a decomposition recipe to contain free variables. 
   * i.e. a child step may use variables not be specified in the parent step. 
   * 
   * Additionally, we now allow the steps in the recipe to contain ground symbols. This could make planning faster. 
   * 
   * Albert Li Aug 17 2012
   */
  protected def extractRecipes(raw: List[RawRecipe], actions: List[DecompAction], problem: Problem): List[Recipe] = {
    
    raw map { r =>

      val tokenTypes = HashMap[String, String]()
      val bound = HashMap[String, List[Token]]()
      val parent = findAction(Symbol(r.name), actions)

      // keeping record of the parent's variable types
      parent.parameters foreach { v =>
        tokenTypes += ((v.name, v.pType))
      }
      
      // find the corresponding action templates, and 
      // modify the templates to use the same variables as in the recipe
      val steps = r.steps map {
        step =>
          val stepNum = step._1.replaceAll("step","").toInt
          val actInput = step._2 // this is actually parsed into a proposition
          val actName = actInput.verb          
          val actTempl = findAction(actName, actions)

          var arguments = ListBuffer[Variable]()

          // First, their size has to be the same
          if (step._2.termlist.size != actTempl.parameters.size)
            throw new PopParsingException("parameter list " + actInput.termlist + " does not match the parameters of action " + actName)
          
          
          val pairs = (actTempl.parameters) zip (actInput.termlist)
          // checking for each pair of matched tokens
          pairs foreach {
            
            case (standard, specified: Variable) =>
              // try to find a type from specified variable
              val typeStored = tokenTypes.get(specified.name)
              val typeSpecified = specified.pType
              val realType = {
                if (typeSpecified == "Any")
                {
                  if (typeStored.isDefined) typeStored.get
                  else {
                    tokenTypes += ((specified.name, standard.pType))
                    standard.pType
                  }
                }
                else
                {
                  // exception case
                  if (typeStored.isDefined && typeStored.get != typeSpecified)
                  {
                    val str = new StringBuilder()
                    str append "For variable " append specified.name append " in decomp " append r.name append " the specified type "
                    str append typeSpecified append "does not match the inferred type " append typeStored.get
                    throw new PopParsingException(str.toString())
                  }
                  else if (typeStored.isEmpty)
                  {
                    tokenTypes += ((specified.name, typeSpecified))
                  }
                    typeSpecified // typeStored == typeSpecified                 
                }
              }
              
                if (realType != standard.pType)  
                {
                  // type mismatch. check if subtype relation exists
                  if (!problem.ontology.isSubtype(realType, standard.pType)) {
                    // no subtype relation exists
                    throw new PopParsingException("Action " + actName + " requires type: " + standard.pType + ", but variable " +
                      specified.name + " is of type " + realType + " in decomposition " + r.name)
                } 
                }
                  
                // add to bound list
                   bound.get(specified.name) match {
                     case Some(existent) => 
                       bound += ((specified.name, standard.instantiate(stepNum) :: existent))
                     case None => bound += ((specified.name, List(standard.instantiate(stepNum))))
                   }
              

            case (v:Variable, obj:PopObject) => // The recipe has specified an object
              // TODO: find the real type of the object
              val realType = {
                if (obj.pType != "Any") obj
                else 
              }
            case (a, b) => // Error if neither above cases are met.
              throw new PopParsingException("this is not a parameter: " + b)
          }

                   

          (stepNum, actTempl)
      }

      // Compute causal links
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

      // Compute orderings
      var orderings = r.ordering map {
        order =>
          val num1 = steps.indexWhere(_._1 == order._1)
          val num2 = steps.indexWhere(_._1 == order._2)
          (num1, num2)
      }
      
      // Add orderings implied by causal links
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
      var problem = ProblemParser.readFile(problemFile)
      var listAction = ActionParser.readFile(actionFile)

      val ontology = problem.ontology
      listAction = listAction map { ontology.appendTypesToTemplate }
      
      // test the action validity for once
      listAction foreach { _.testValid() }

      (problem, listAction)
    }

  /*
  def parseProblem(problemFile: String): Problem =
    {
      objectHash.clear()
      var problem = ProblemParser.readFile(problemFile)
      problem.init foreach { collectTypes(_) }
      problem.goal foreach { collectTypes(_) }
      val init = problem.init map { appendTypesTo(_) }
      val goal = problem.goal map { appendTypesTo(_) }
      new Problem(init, goal, problem.ontology)
    }*/

  /*
  /** This method tries to be a more stable version of extract recipe but fails
   * The problem with the old extract recipe is that when the parent has the variables (v1, v2)
   * and the child action is specified with (v2, v1) 
   */
  protected def extractRecipesBuggy(raw: List[RawRecipe], actions: List[DecompAction], problem: Problem): List[Recipe] = {
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

          /* Preparing the lists for substitution. 
           * We have to give the old parameters an prefix "$", 
           * so that the old variables and new variables can be differentiated.
           */
          for (v <- step._2.allVariables()) {
            val prefixVar = new Variable("$" + v.name, v.pType)
            constraints = constraints map { _.substitute(v, prefixVar) }
            preconds = preconds map { _.substitute(v, prefixVar) }
            effects = effects map { _.substitute(v, prefixVar) }
          }

          // match the variables from the recipe (step._2.termlist) with the parameters from the action template 
          if (step._2.termlist.size != act.parameters.size)
            throw new PopParsingException("parameter list does not match for action " + step._2.verb)
          val pairs = act.parameters zip (step._2.termlist)

          pairs foreach {

            case (standard, specified: Variable) =>
              // the corresponding variable from the parent action
              val parentParameter = parent.parameters.find(_.name == specified.name)
              if (parentParameter.isEmpty) {
                /* this variable is not used in the parent action. 
                 * Thus, it is a *free* variable 
                 * This is disallowed. Throw an exception
                 */
                throw new PopParsingException("Warning: " + specified + " of step " + name + " is a free variable in decomposition " + r.name + ".")
              } else {
                var newVar: Variable = null
                val specifiedType = parentParameter.get.pType
                if (specifiedType != standard.pType) {
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
                if (newVar == null) throw new RuntimeException("new variable == null. Something is wrong.")
                val oldVar = new Variable("$" + standard.name, standard.pType)
                constraints = constraints map { _.substitute(oldVar, newVar) }
                preconds = preconds map { _.substitute(oldVar, newVar) }
                effects = effects map { _.substitute(oldVar, newVar) }
                
//                println("constraints = " + constraints)
//                println("preconds = " + preconds)
//                println("effects = " + effects)
              }

            case (a, b) => // reaching this case means the second parameter is not a variable
              throw new PopParsingException("this is not a parameter: " + b)
          }

          // now this step has passed the type check
          // substituting the variables in the action template with variables in the parent

          val newAct = DecompAction(act.name, arguments.toList, constraints, preconds, effects, act.composite)

          (step._1, newAct)
      }

      // Compute causal links
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

      // Compute orderings
      var orderings = r.ordering map {
        order =>
          val num1 = steps.indexWhere(_._1 == order._1)
          val num2 = steps.indexWhere(_._1 == order._2)
          (num1, num2)
      }

      // Add orderings implied by causal links
      orderings = (links.map { l => (l.id1, l.id2) } ::: orderings).distinct

      new Recipe(r.name, steps map { _._2 }, links, orderings)
    }
  } */
}