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

    def fromString(input: String, context: Map[String, Type]): Option[Application] = {
        val updatedInput = util.trimBrackets(input)
        if (!updatedInput.exists(_ == ' ') || !util.isBalanced(updatedInput))
            return None

        util.computeBalances(updatedInput).zipWithIndex.findLast { case (balance, index) =>
            balance == 0 && updatedInput(index) == ' '
        }.flatMap { case (_, index) =>
            // trying to parse both parts of the application
            val (left, right) = splitByIndex(updatedInput, index)
            for {
                leftTerm <- Term.fromString(left, context)
                rightTerm <- Term.fromString(right, context)
            } yield Application(leftTerm, rightTerm)
        }
    }

    private def splitByIndex(input: String, index: Int): (String, String) =
        (input.take(index), input.takeRight(input.length - index - 1))
}
