package types

case class TypeVariable(name: String) extends Type() {

    override def toString: String = name

    override def toStringWithBrackets: String = name

    override def applyTo(other: Type): Option[Type] = None

    override def substitute(previous: TypeVariable, next: Type): Type =
        if (this == previous)
            next
        else
            this
}
