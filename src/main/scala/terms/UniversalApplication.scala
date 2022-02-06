package terms

import types._

case class UniversalApplication(left: Term, right: Type) extends Term {

    override def toString = s"$left ~ ${right.toStringWithBrackets}"

    override def toStringVerbose =
        s"UniversalApplication(${left.toStringVerbose}, ${right.toStringVerbose})"

    override def getType: Option[Type] =
        left.getType.flatMap {
            case leftType@TypeAbstraction(typeVariable, _) =>
                Some(leftType.substitute(typeVariable, right))
            case _ => None
        }
}
