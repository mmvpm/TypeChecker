package types

import org.scalatest.flatspec.AnyFlatSpec

class TypeTest extends AnyFlatSpec {

    "Type.fromString" should "parse valid input to Type" in new Wiring {
        val validTestCases = Map(
            "a" -> tcA,
            "A" -> tcbA,
            "alpha" -> tcAlpha,
            "Alpha" -> tcbAlpha,
            "a => b" -> arr(tcA, tcB),
            "alpha => beta" -> arr(tcAlpha, tcBeta),
            "alpha => (beta => gamma)" -> arr(tcAlpha, arr(tcBeta, tcGamma)),
            "(alpha => beta) => gamma" -> arr(arr(tcAlpha, tcBeta), tcGamma),
            "(alpha => beta => gamma)" -> arr(tcAlpha, arr(tcBeta, tcGamma)),
            "(a) => (b) => (a)" -> arr(tcA, arr(tcB, tcA)),
            "(a => (b => c) => a)" -> arr(tcA, arr(arr(tcB, tcC), tcA)),
            "(a => ((b => c)) => a)" -> arr(tcA, arr(arr(tcB, tcC), tcA)),
            "a => (a => (a => a)) => a" -> arr(tcA, arr(arr(tcA, arr(tcA, tcA)), tcA))
        )

        validTestCases.foreach { case (input, expected) =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Type.fromString" should "return None on invalid input" in new Wiring {
        val invalidTestCases = List(
            "",
            " ",
            ")",
            "(",
            "=> )",
            "a b",
            "a = b",
            "a > b",
            "a => ",
            "a ( => b)",
            "a -> b -> a",
            "a (=>) b => a",
            "alpha ( => ) beta",
            "a (b => c) => d",
        )

        invalidTestCases.foreach { input =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt.isEmpty)
        }
    }

    "applyTo" should "correctly apply first type to compatible second type" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("a => a", "a", "a"),
            List("a => b", "a", "b"),
            List("a => b => c", "a", "b => c"),
            List("(a => b) => c", "a => b", "c"),
            List("(a => b => c) => a => b", "a => b => c", "a => b"),
            List("a => (a => a) => a", "a", "(a => a) => a"),
            List("(a => (a => a)) => (a => a) => a", "a => (a => a)", "(a => a) => a"),
        ).map(_.map(Type.fromString(_).get))

        validTestCases.foreach { case List(first, second, expected) =>
            val actualOpt = first.applyTo(second)
            assert(actualOpt contains expected)
        }
    }

    "applyTo" should "fail apply first type to incompatible second type" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("a => a", "b"),
            List("a => b", "b"),
            List("a => b => c", "a => b"),
            List("(a => b) => c", "c"),
            List("(a => b => c) => a => b", "a => b"),
            List("a => (a => a) => a", "b"),
            List("(a => (a => a)) => (a => a) => a", "a"),
        ).map(_.map(Type.fromString(_).get))

        validTestCases.foreach { case List(first, second) =>
            val actualOpt = first.applyTo(second)
            assert(actualOpt.isEmpty)
        }
    }

    trait Wiring {
        val tcA = TypeConstant("a")
        val tcbA = TypeConstant("A")
        val tcB = TypeConstant("b")
        val tcC = TypeConstant("c")
        val tcAlpha = TypeConstant("alpha")
        val tcbAlpha = TypeConstant("Alpha")
        val tcBeta = TypeConstant("beta")
        val tcGamma = TypeConstant("gamma")

        // function with short name for convenience

        def arr: (Type, Type) => TypeArrow = TypeArrow(_, _)
    }
}
