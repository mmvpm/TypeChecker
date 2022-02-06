package types

case class TypeConstant(name: String) extends Type {

    override def toString: String = name

    override def toStringWithBrackets: String = name

    override def toStringVerbose = s"TypeConstant($name)"

    override def applyTo(other: Type): Option[Type] = None

    override def substitute(previous: TypeVariable, next: Type): Type = this
}

object TypeConstant {

    def fromString(input: String, typeVariables: Set[String]): Option[TypeConstant] = {
        val updatedInput = util.trimBrackets(input)

        if (updatedInput.isEmpty || updatedInput.exists(".:-=>( )" contains _))
            return None // invalid input
        if (typeVariables.contains(updatedInput))
            return None // double intro

        Some(TypeConstant(updatedInput))
    }
}
