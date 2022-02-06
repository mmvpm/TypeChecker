package types

case class TypeVariable(name: String) extends Type() {

    override def toString: String = name

    override def toStringWithBrackets: String = name

    override def toStringVerbose = s"TypeVariable($name)"

    override def applyTo(other: Type): Option[Type] = None

    override def substitute(previous: TypeVariable, next: Type): Type =
        if (this == previous)
            next
        else
            this
}

object TypeVariable {

    def fromString(input: String, typeVariables: Set[String]): Option[TypeVariable] = {
        val updatedInput = util.trimBrackets(input)

        if (!typeVariables.contains(updatedInput))
            return None // unknown type variable

        Some(TypeVariable(updatedInput))
    }
}
