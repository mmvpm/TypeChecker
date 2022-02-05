package types

import scala.collection.mutable

abstract class Type {

    def applyTo(other: Type): Option[Type]

    def toStringWithBrackets: String

    def substitute(previous: TypeVariable, next: Type): Type
}

object Type {

    def fromString(input: String): Option[Type] = {
        // for easier parsing: "(a -> b) -> c -> d" => "(a>b)>c>d>"
        val updatedInput = input.filterNot(" -" contains _) + ">"

        val stack = new SafeStack() // just for convenience
        val lastName = new StringBuilder()

        for (symbol <- updatedInput) {
            if ("(>)".contains(symbol) && lastName.nonEmpty) {
                val name = lastName.toString()
                stack.push(Right(TypeConstant(name))) // new type constant
                lastName.clear()
            }
            symbol match {
                case '(' =>
                    stack.push(Left("("))
                case '>' =>
                    () // do nothing
                case ')' =>
                    stack.fold() // folding the stack before the first opening bracket
                    if (stack.safeTop.isEmpty)
                        return None // parsing fails
                case _ =>
                    lastName += symbol
            }
        }

        stack.fold()
        stack.safeTop
    }

    // internal

    private class SafeStack() {

        private val stack = mutable.Stack[Either[String, Type]]() // "(" or type

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

            // collecting resultType from the stack
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