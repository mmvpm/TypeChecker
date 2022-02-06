package terms

import types._

case class Abstraction(variable: Variable, term: Term) extends Term {

    override def toString = s"\\${variable.toStringTyped} -> $term"

    override def toStringVerbose =
        s"Abstraction(${variable.toStringVerbose}, ${term.toStringVerbose})"

    override def getType: Option[Type] =
        term.getType.map { bodyType =>
            TypeArrow(variable.`type`, bodyType)
        }
}

object Abstraction {

    def fromString(
        input: String,
        context: Map[String, Type],
        typeVariables: Set[String]
    ): Option[Abstraction] = {
        val (name, typeInput, bodyInput) = util.trimBrackets(input) match {
            case s"""\$name:$typeInput->$bodyInput""" =>
                (name.trim, typeInput.trim, bodyInput.trim)
            case _ =>
                return None
        }
        if (!util.validateName(name))
            return None // invalid name

        for {
            nameType <- Type.fromString(typeInput, typeVariables)
            variable = Variable(name, nameType)
            if !context.contains(name) // double intro
            updatedContext = context + (name -> nameType)
            term <- Term.fromString(bodyInput, updatedContext, typeVariables)
        } yield Abstraction(variable, term)
    }
}
