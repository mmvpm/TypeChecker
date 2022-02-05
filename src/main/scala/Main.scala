import terms._
import types._

object Main extends App {
//    val example = "\\f: alpha -> alpha . \\a: alpha . f (f a)"
//    val inputPrompt = s"Example: \"$example\"\nEnter lambda to check the type: "
//
//    val input = scala.io.StdIn.readLine(inputPrompt)
//
//    Term
//        .fromString(input)
//        .map(_.getType) match {
//            case Some(Some(value)) =>
//                println(s"Type: $value")
//            case Some(None) =>
//                println("Type check failed")
//            case None =>
//                println("Invalid term")
//        }

    val term = UniversalAbstraction(
        TypeVariable("alpha"),
        Abstraction(Variable("a", TypeVariable("alpha")), Variable("a", TypeVariable("alpha")))
    )
    println(term)
    println(term.getType)
}
