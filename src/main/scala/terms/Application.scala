package terms

import types._

case class Application(left: Term, right: Term) extends Term {

    override def toString: String = s"$left ${right.toStringWithBrackets}"

    override def toStringWithBrackets: String = s"($toString)"

    override def toStringVerbose: String =
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
        val updatedInput = Term.trimBrackets(input) + " " // +1 `for` iteration

        if (updatedInput.length < 4) // min application example: "a b "
            return None

        var bracketBalance = 0 // checking for the balanced bracket sequence
        var preLastZeroBalance: Option[Int] = None
        var lastZeroBalance: Option[Int] = None

        for ((symbol, index) <- updatedInput.zipWithIndex) {
            symbol match {
                case '(' => bracketBalance += 1
                case ')' => bracketBalance -= 1
                case _ => () // do nothing
            }
            if (bracketBalance < 0)
                return None // incorrect brackets

            if (symbol == ' ' && bracketBalance == 0) {
                preLastZeroBalance = lastZeroBalance
                lastZeroBalance = Some(index)
            }
        }
        if (bracketBalance != 0)
            return None // incorrect brackets

        // "(a b) c d"
        //       ^ - `preLastZeroBalance`
        preLastZeroBalance.flatMap { index =>
            for {
                // trying to parse both parts of the application
                left <- Term.fromString(
                    updatedInput.take(index + 1),
                    context
                )
                right <- Term.fromString(
                    updatedInput.takeRight(updatedInput.length - index - 1),
                    context
                )
            } yield Application(left, right)
        }
    }
}
