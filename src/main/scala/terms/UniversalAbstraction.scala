package terms

import types._

case class UniversalAbstraction(typeVariable: TypeVariable, term: Term) extends Term {

    override def toString: String = s"forall $typeVariable . $term"

    override def toStringWithBrackets: String = s"($toString)"

    override def toStringVerbose: String =
        s"TypeAbstraction($typeVariable, ${term.toStringVerbose})"

    override def getType: Option[Type] =
        term.getType.map { bodyType =>
            TypeAbstraction(typeVariable, bodyType)
        }
}
