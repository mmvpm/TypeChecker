package types

case class TypeAbstraction(typeVariable: TypeVariable, typeTerm: Type) extends Type() {

    override def toString: String = s"forall $typeVariable . $typeTerm"

    override def toStringWithBrackets: String = s"($toString)"

    override def applyTo(other: Type): Option[Type] = None

    override def substitute(previous: TypeVariable, next: Type): Type =
        TypeAbstraction(typeVariable, typeTerm.substitute(previous, next))
}
