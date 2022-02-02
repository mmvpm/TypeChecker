package expressions

import types._

case class Application(left: Expression, right: Expression) extends Expression {

    override def toStringWithBrackets: String = s"($toString)"

    override def toString: String = s"$left ${right.toStringWithBrackets}"

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
        val updatedInput = Expression.trimBrackets(input) + " " // +1 `for` iteration

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
                left <- Expression.fromString(
                    updatedInput.take(index + 1),
                    context
                )
                right <- Expression.fromString(
                    updatedInput.takeRight(updatedInput.length - index - 1),
                    context
                )
            } yield Application(left, right)
        }
    }
}
