import terms._

object Main extends App {
    val example = "forall alpha -> \\f: forall beta . beta => alpha -> \\x: Bool -> f ~ Bool x"
    val inputPrompt = s"Example: \"$example\"\nEnter lambda to check the type: "

    val input = scala.io.StdIn.readLine(inputPrompt)

    Term
        .fromString(input)
        .map(_.getType) match {
            case Some(Some(value)) =>
                println(s"Type: $value")
            case Some(None) =>
                println("Type check failed")
            case None =>
                println("Invalid term")
        }
}
