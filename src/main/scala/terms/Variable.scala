package terms

import types._

case class Variable(name: String, `type`: Type) extends Term {

    override def toString: String = name

    def toStringTyped = s"$name: ${`type`}"

    override def toStringWithBrackets: String = name

    override def toStringVerbose = s"Variable($name, ${`type`.toStringVerbose})"

    override def getType: Option[Type] = Some(`type`)
}

object Variable {

    def fromString(
        input: String,
        context: Map[String, Type],
        typeVariables: Set[String]
    ): Option[Variable] = {
        val name = util.trimBrackets(input)

        context.get(name).map { `type` =>
            Variable(name, `type`)
        }
    }
}
