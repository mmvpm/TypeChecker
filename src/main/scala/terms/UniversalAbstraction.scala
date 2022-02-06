package terms

import types._

case class UniversalAbstraction(typeVariable: TypeVariable, term: Term) extends Term {

    override def toString = s"∀$typeVariable -> $term"

    override def toStringVerbose =
        s"UniversalAbstraction(${typeVariable.toStringVerbose}, ${term.toStringVerbose})"

    override def getType: Option[Type] =
        term.getType.map { bodyType =>
            TypeAbstraction(typeVariable, bodyType)
        }
}

object UniversalAbstraction {

    def fromString(
        input: String,
        context: Map[String, Type],
        typeVariables: Set[String]
    ): Option[UniversalAbstraction] = {
        val (name, bodyInput) = util.trimBrackets(input) match {
            case s"forall $name->$bodyInput" =>
                (name.trim, bodyInput.trim)
            case _ =>
                return None
        }
        if (!util.validateName(name))
            return None // invalid name
        if (typeVariables.contains(name))
            return None // double intro

        Term.fromString(bodyInput, context, typeVariables + name).map { term =>
            UniversalAbstraction(TypeVariable(name), term)
        }
    }
}
