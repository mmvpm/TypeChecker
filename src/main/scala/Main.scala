import expressions._

object Main extends App {
    val example = "\\f: alpha -> alpha . \\a: alpha . f (f a)"
    val inputPrompt = s"Example: \"$example\"\nEnter lambda to check the type: "

    val input = scala.io.StdIn.readLine(inputPrompt)

    Expression
        .fromString(input)
        .map(_.getType) match {
            case Some(Some(value)) =>
                println(s"Type: $value")
            case Some(None) =>
                println("Type check failed")
            case None =>
                println("Invalid expression")
        }
}
