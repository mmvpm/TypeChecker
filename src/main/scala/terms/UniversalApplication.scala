package terms

import types._

case class UniversalApplication(left: Term, right: Type) extends Term {

    override def toString: String = s"$left ${right.toStringWithBrackets}"

    override def toStringWithBrackets: String = s"($toString)"

    override def toStringVerbose: String =
        s"UniversalApplication(${left.toStringVerbose}, $right)"

    override def getType: Option[Type] =
        left match {
            case UniversalAbstraction(typeVariable, _) =>
                left.getType.map { typeAbstraction =>
                    typeAbstraction.substitute(typeVariable, right)
                }
            case _ => None
        }
}
