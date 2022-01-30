package types

case class TypeArrow(left: Type, right: Type) extends Type {

    override def toString: String =
        s"${left.toStringWithBrackets} -> $right"

    override def toStringWithBrackets: String =
        s"($left -> $right)"

    override def applyTo(other: Type): Option[Type] =
        if (other == left)
            Some(right)
        else
            None
}
