package terms

import types._

case class Abstraction(variable: Variable, term: Term) extends Term {

    override def toString: String = s"\\${variable.toStringTyped} . $term"

    override def toStringWithBrackets: String = s"($toString)"

    override def toStringVerbose: String =
        s"Abstraction(${variable.toStringVerbose}, ${term.toStringVerbose})"

    override def getType: Option[Type] =
        term.getType.map { bodyType =>
            TypeArrow(variable.`type`, bodyType)
        }
}

object Abstraction {

    def fromString(input: String, context: Map[String, Type]): Option[Abstraction] = {
        val updatedInput = Term.trimBrackets(input)

        if (updatedInput.length < 6 || updatedInput(0) != '\\') // min abstraction example: "\a:a.a"
            return None

        // split input by ":" and "." signs
        val typeStart = updatedInput.indexOf(':')
        val typeEnd = updatedInput.indexOf('.')
        if (typeStart == -1 || typeEnd == -1)
            return None

        // \name: typeInput . bodyInput
        val name = updatedInput.slice(1, typeStart).trim
        val typeInput = updatedInput.slice(typeStart + 1, typeEnd).trim
        val bodyInput = updatedInput.slice(typeEnd + 1, updatedInput.length).trim

        for {
            nameType <- Type.fromString(typeInput)
            variable = Variable(name, nameType)
            if !context.contains(name) // against double intro
            updatedContext = context + (name -> nameType)
            term <- Term.fromString( // almost a recursive call
                bodyInput,
                updatedContext
            )
        } yield Abstraction(variable, term)
    }
}