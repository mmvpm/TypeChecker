package terms

import types._

case class Variable(name: String, `type`: Type) extends Term {

    override def toString: String = name

    def toStringTyped: String = s"$name: ${`type`}"

    override def toStringWithBrackets: String = name

    override def toStringVerbose: String = s"Variable($name, ${`type`})"

    override def getType: Option[Type] = Some(`type`)
}

object Variable {

    def fromString(input: String, context: Map[String, Type]): Option[Variable] = {
        val name = Term.trimBrackets(input)

        if (name.isEmpty)
            return None

        if (name contains ' ')
            return None

        context.get(name).map { `type` =>
            Variable(name, `type`)
        }
    }
}
