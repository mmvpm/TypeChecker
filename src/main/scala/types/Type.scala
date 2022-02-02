package types

import scala.collection.mutable

abstract class Type {

    def applyTo(other: Type): Option[Type]

    def toStringWithBrackets: String
}

object Type {

    def fromString(input: String): Option[Type] = {
        // for easier parsing: "(a -> b) -> c -> d" => "(a>b)>c>d>"
        val updatedInput = input.filterNot(" -" contains _) + ">"

        val stack = new SafeStack()
        val lastName = new StringBuilder()

        for (symbol <- updatedInput) {
            if ("(>)".contains(symbol) && lastName.nonEmpty) {
                val name = lastName.toString()
                stack.push(Right(TypeConstant(name)))
                lastName.clear()
            }
            symbol match {
                case '(' =>
                    stack.push(Left("("))
                case '>' =>
                    () // do nothing
                case ')' =>
                    stack.fold()
                    if (stack.safeTop.isEmpty)
                        return None
                case _ =>
                    lastName += symbol
            }
        }

        stack.fold()
        stack.safeTop
    }

    // internal

    private class SafeStack() {

        private val stack = mutable.Stack[Either[String, Type]]()

        def push(element: Either[String, Type]): Unit =
            stack.push(element)

        def safeTop: Option[Type] = {
            if (stack.isEmpty)
                return None
            stack.top.toOption
        }

        def fold(): Unit = {
            if (stack.isEmpty || stack.top.isLeft)
                return

            var resultType = stack.pop().right.get
            while (stack.nonEmpty && stack.top.isRight) {
                val prevType = stack.pop.right.get
                resultType = TypeArrow(prevType, resultType)
            }

            if (stack.nonEmpty && stack.top.isLeft)
                stack.pop() // pop Left("(") if exists

            stack.push(Right(resultType))
        }
    }
}