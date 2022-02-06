package terms

import types._

case class Application(left: Term, right: Term) extends Term {

    override def toString = s"$left ${right.toStringWithBrackets}"

    override def toStringVerbose =
        s"Application(${left.toStringVerbose}, ${right.toStringVerbose})"

    override def getType: Option[Type] =
        for {
            leftType <- left.getType
            rightType <- right.getType
            resultType <- leftType.applyTo(rightType)
        } yield resultType
}

object Application {

    def fromString(
        input: String,
        context: Map[String, Type],
        typeVariables: Set[String]
    ): Option[Term] = { // constructs Application or UniversalApplication
        val updatedInput = util.trimBrackets(input)
        if (!updatedInput.exists(" ~" contains _) || !util.isBalanced(updatedInput))
            return None

        util.computeBalances(updatedInput).zipWithIndex.findLast { case (balance, index) =>
            balance == 0 && index > 0 && (
                // it's enough to check (i - 1)-th due to split & mkString in `Term.fromString`
                updatedInput(index) == ' ' && updatedInput(index - 1) != '~' ||
                updatedInput(index) == '~'
            )
        }.flatMap { case (_, index) =>
            // trying to parse both parts of the (universal) application
            val (left, right) = splitByIndex(updatedInput, index)
            if (updatedInput(index) == '~')
                for {
                    leftTerm <- Term.fromString(left, context, typeVariables)
                    rightType <- Type.fromString(right, typeVariables)
                } yield UniversalApplication(leftTerm, rightType)
            else // updatedInput(index) == ' '
                for {
                    leftTerm <- Term.fromString(left, context, typeVariables)
                    rightTerm <- Term.fromString(right, context, typeVariables)
                } yield Application(leftTerm, rightTerm)
        }
    }

    private def splitByIndex(input: String, index: Int): (String, String) =
        (input.take(index), input.takeRight(input.length - index - 1))
}
