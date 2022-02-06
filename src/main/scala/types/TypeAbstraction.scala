package types

case class TypeAbstraction(typeVariable: TypeVariable, typeTerm: Type) extends Type() {

    override def toString = s"∀$typeVariable . $typeTerm"

    override def toStringVerbose =
        s"TypeAbstraction(${typeVariable.toStringVerbose}, ${typeTerm.toStringVerbose})"

    override def applyTo(other: Type): Option[Type] = None

    override def substitute(previous: TypeVariable, next: Type): Type =
        if (previous == typeVariable)
            typeTerm.substitute(previous, next)
        else
            TypeAbstraction(typeVariable, typeTerm.substitute(previous, next))
}

object TypeAbstraction {

    def fromString(input: String, typeVariables: Set[String]): Option[TypeAbstraction] = {
        val (name, typeInput) = util.trimBrackets(input) match {
            case s"forall $name.$typeInput" =>
                (name.trim, typeInput.trim)
            case _ =>
                return None
        }
        if (!util.validateName(name))
            return None // invalid name
        if (typeVariables.contains(name))
            return None // double intro

        Type.fromString(typeInput, typeVariables + name).map { `type` =>
            TypeAbstraction(TypeVariable(name), `type`)
        }
    }
}
