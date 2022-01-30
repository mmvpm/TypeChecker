package expressions

import types._

case class Abstraction(variable: Variable, expression: Expression) extends Expression {

    override def toStringWithBrackets: String = s"($toString)"

    override def toString: String = s"\\${variable.toStringTyped} . $expression"

    override def toStringVerbose: String =
        s"Abstraction(${variable.toStringVerbose}, ${expression.toStringVerbose})"

    override def getType: Option[Type] =
        expression.getType.map { bodyType =>
            TypeArrow(variable.`type`, bodyType)
        }
}

object Abstraction {

    def fromString(input: String, context: Map[String, Type]): Option[Abstraction] = {
        val updatedInput = Expression.trimBrackets(input)

        if (updatedInput.length < 6 || updatedInput(0) != '\\') // min abstraction example: "\a:a.a"
            return None

        val typeStart = updatedInput.indexOf(':')
        val typeEnd = updatedInput.indexOf('.')
        if (typeStart == -1 || typeEnd == -1)
            return None

        val name = updatedInput.slice(1, typeStart).trim
        val typeInput = updatedInput.slice(typeStart + 1, typeEnd).trim
        val bodyInput = updatedInput.slice(typeEnd + 1, updatedInput.length).trim

        for {
            variableType <- Type.fromString(typeInput)
            variable = Variable(name, variableType)
            if !context.contains(name) // against double intro
            updatedContext = context + (name -> variableType)
            expression <- Expression.fromString(
                bodyInput,
                updatedContext
            )
        } yield Abstraction(variable, expression)
    }
}