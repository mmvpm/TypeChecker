package types

import org.scalatest.flatspec.AnyFlatSpec

class TypeTest extends AnyFlatSpec {

    "Type.fromString" should "parse valid input to Type" in new Wiring {
        val validTestCases = Map(
            "a" -> tvA,
            "A" -> tvbA,
            "alpha" -> tvAlpha,
            "Alpha" -> tvbAlpha,
            "a -> b" -> TypeArrow(tvA, tvB),
            "alpha -> beta" -> TypeArrow(tvAlpha, tvBeta),
            "alpha -> (beta -> gamma)" -> TypeArrow(tvAlpha, TypeArrow(tvBeta, tvGamma)),
            "(alpha -> beta) -> gamma" -> TypeArrow(TypeArrow(tvAlpha, tvBeta), tvGamma),
            "(alpha -> beta -> gamma)" -> TypeArrow(tvAlpha, TypeArrow(tvBeta, tvGamma)),
            "(a) -> (b) -> (a)" -> TypeArrow(tvA, TypeArrow(tvB, tvA)),
            "(a -> (b -> c) -> a)" -> TypeArrow(tvA, TypeArrow(TypeArrow(tvB, tvC), tvA)),
            "(a -> ((b -> c)) -> a)" -> TypeArrow(tvA, TypeArrow(TypeArrow(tvB, tvC), tvA)),
            "a -> (a -> (a -> a)) -> a" -> TypeArrow(tvA, TypeArrow(TypeArrow(tvA, TypeArrow(tvA, tvA)), tvA))
        )

        validTestCases.foreach { case (input, expected) =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Type.fromString" should "return None on invalid input" in new Wiring {
        val invalidTestCases = List(
            "",
            "(",
            ")",
            "-> )",
            "( -> )",
            "a ( -> )",
            "alpha ( -> )",
            "alpha ( -> ) beta",
        )

        invalidTestCases.foreach { input =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt.isEmpty)
        }
    }

    "applyTo" should "correctly apply first type to compatible second type" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("a -> a", "a", "a"),
            List("a -> b", "a", "b"),
            List("a -> b -> c", "a", "b -> c"),
            List("(a -> b) -> c", "a -> b", "c"),
            List("(a -> b -> c) -> a -> b", "a -> b -> c", "a -> b"),
            List("a -> (a -> a) -> a", "a", "(a -> a) -> a"),
            List("(a -> (a -> a)) -> (a -> a) -> a", "a -> (a -> a)", "(a -> a) -> a"),
        ).map(_.map(Type.fromString(_).get))

        validTestCases.foreach { case List(first, second, expected) =>
            val actualOpt = first.applyTo(second)
            assert(actualOpt contains expected)
        }
    }

    "applyTo" should "fail apply first type to incompatible second type" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("a -> a", "b"),
            List("a -> b", "b"),
            List("a -> b -> c", "a -> b"),
            List("(a -> b) -> c", "c"),
            List("(a -> b -> c) -> a -> b", "a -> b"),
            List("a -> (a -> a) -> a", "b"),
            List("(a -> (a -> a)) -> (a -> a) -> a", "a"),
        ).map(_.map(Type.fromString(_).get))

        validTestCases.foreach { case List(first, second) =>
            val actualOpt = first.applyTo(second)
            assert(actualOpt.isEmpty)
        }
    }

    trait Wiring {
        val tvA = TypeVariable("a")
        val tvbA = TypeVariable("A")
        val tvB = TypeVariable("b")
        val tvC = TypeVariable("c")
        val tvAlpha = TypeVariable("alpha")
        val tvbAlpha = TypeVariable("Alpha")
        val tvBeta = TypeVariable("beta")
        val tvGamma = TypeVariable("gamma")
    }
}
