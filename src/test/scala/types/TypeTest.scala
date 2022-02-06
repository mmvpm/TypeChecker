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

    // System F

    "Type.fromString" should "parse valid input to System F Type" in new Wiring {
        val validTestCases = List(
            "forall alpha . alpha" -> lam(tvA, tvA),
            "forall alpha . alpha => alpha" -> lam(tvA, arr(tvA, tvA)),
            "forall alpha . alpha => forall beta . a" -> lam(tvA, arr(tvA, lam(tvB, tcA))),
            "forall alpha . forall beta . alpha => beta" -> lam(tvA, lam(tvB, arr(tvA, tvB))),
            "forall alpha . alpha => forall beta . (alpha => beta) => alpha" -> lam(tvA, arr(tvA, lam(tvB, arr(arr(tvA, tvB), tvA)))),
            "forall alpha . alpha => forall beta . a => alpha => beta => b" -> lam(tvA, arr(tvA, lam(tvB, arr(tcA, arr(tvA, arr(tvB, tcB)))))),
            "c => forall alpha . alpha => forall beta . (alpha => beta) => alpha" -> arr(tcC, lam(tvA, arr(tvA, lam(tvB, arr(arr(tvA, tvB), tvA)))))
        )

        validTestCases.foreach { case (input, expected) =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Type.fromString" should "return None on invalid System F input" in new Wiring {
        val validTestCases = List(
            "forall alpha  alpha", // no "."
            "foral alpha . alpha => alpha", // "foral" instead of "forall"
            "forall alpha . alpha -> forall beta . a", // "->" instead of "=>"
            "forall alpha . forall beta -> alpha => beta", // "->" instead of "."
            "forall alpha . alpha => forall . (alpha => beta) => alpha", // empty variable name
            "forall alpha . alpha => forall beta .  => alpha => beta => b", // empty constant name
            "c  forall alpha . alpha => forall beta . (alpha => beta) => alpha" // no "=>"
        )

        validTestCases.foreach { input =>
            val actualOpt = Type.fromString(input)
            assert(actualOpt.isEmpty)
        }
    }

    "substitute" should "replace types correctly" in new Wiring {
        val testCases = List(
            (tcA, tvA, tcB, tcA),
            (tvA, tvA, tcA, tcA),
            (tvA, tvB, tcA, tvA),
            (lam(tvA, tvA), tvB, tcA, lam(tvA, tvA)),
            (lam(tvA, tvB), tvB, tcA, lam(tvA, tcA)),
            (arr(tvA, tvA), tvA, tcA, arr(tcA, tcA)),
            (arr(tvA, tvA), tvB, tcA, arr(tvA, tvA)),
        )

        testCases.foreach { case (sourceType, typeVariable, nextType, expected) =>
            val actual = sourceType.substitute(typeVariable, nextType)
            assert(actual == expected)
        }
    }

    "applyTo" should "correctly apply compatible System F types" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("(forall alpha . alpha) => alpha", "forall alpha . alpha", "alpha"),
            List("(forall alpha . alpha) => a", "forall alpha . alpha", "a"),
            List("a => forall alpha . alpha", "a", "forall alpha . alpha"),
            List("(forall alpha . alpha => alpha) => c", "forall alpha . alpha => alpha", "c"),
            List("(forall alpha . alpha => alpha) => (forall alpha . alpha) => a", "forall alpha . alpha => alpha", "(forall alpha . alpha) => a"),
        ).map(_.map(Type.fromString(_).get))

        validTestCases.foreach { case List(first, second, expected) =>
            val actualOpt = first.applyTo(second)
            assert(actualOpt contains expected)
        }
    }

    "applyTo" should "fail apply incompatible System F types" in new Wiring {
        val validTestCases: List[List[Type]] = List(
            List("forall alpha . alpha => alpha", "forall alpha . alpha"),
            List("forall alpha . alpha => a", "forall alpha . alpha"),
            List("a => forall alpha . alpha", "forall alpha . alpha"),
            List("forall alpha . alpha => alpha => c", "forall alpha . alpha => alpha"),
            List("(forall alpha . alpha => alpha) => forall alpha . alpha => a", "alpha"),
            List("(forall alpha . alpha => alpha) => forall alpha . alpha => a", "forall alpha . alpha"),
            List("(forall alpha . alpha => alpha) => forall alpha . alpha => a", "forall alpha . alpha => alpha => alpha"),
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

        val tvA = TypeVariable("alpha")
        val tvB = TypeVariable("beta")

        // functions with short names for convenience

        def arr: (Type, Type) => TypeArrow = TypeArrow(_, _)

        def lam: (TypeVariable, Type) => TypeAbstraction = TypeAbstraction(_, _)
    }
}
