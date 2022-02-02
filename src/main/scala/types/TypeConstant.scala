package types

case class TypeConstant(name: String) extends Type {

    override def toString: String = name

    override def toStringWithBrackets: String = name

    override def applyTo(other: Type): Option[Type] = None
}
