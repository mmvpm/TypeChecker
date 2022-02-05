package terms

import types._

abstract class Term {

    def getType: Option[Type]

    def toStringWithBrackets: String

    def toStringVerbose: String
}

object Term {

    def fromString(input: String, context: Map[String, Type] = Map.empty): Option[Term] = {
        val updatedInput = Term.trimBrackets(input)

        // choose right constructor
        val abstractionOpt = Abstraction.fromString(updatedInput, context)
        if (abstractionOpt.nonEmpty)
            return abstractionOpt

        val variableOpt = Variable.fromString(updatedInput, context)
        if (variableOpt.nonEmpty)
            return variableOpt

        val applicationOpt = Application.fromString(updatedInput, context)
        if (applicationOpt.nonEmpty)
            return applicationOpt

        None // parsing fails
    }

    def trimBrackets(input: String): String = {
        val updatedInput = input.trim
        if (updatedInput.nonEmpty && updatedInput(0) == '(' && updatedInput.last == ')')
            updatedInput.drop(1).dropRight(1)
        else
            updatedInput
    }
}