package types

case class TypeConstant(name: String) extends Type {

    override def toString: String = name

    override def toStringWithBrackets: String = name

    override def toStringVerbose = s"TypeConstant($name)"

    override def applyTo(other: Type): Option[Type] = None
}

object TypeConstant {

    def fromString(input: String): Option[TypeConstant] = {
        val updatedInput = util.trimBrackets(input)

        if (!util.validateName(updatedInput))
            return None // invalid name

        Some(TypeConstant(updatedInput))
    }
}
