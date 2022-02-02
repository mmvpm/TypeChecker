package expressions

import org.scalatest.flatspec.AnyFlatSpec
import types._

class ExpressionTest extends AnyFlatSpec {

    "Expression.fromString" should "parse valid input to Expression" in new Wiring {
        val validInputs = List(
            "\\a: alpha . a" -> lam(vA, vA),
            "\\A: alpha . A" -> lam(vbA, vbA),
            "\\Alpha: alpha . Alpha" -> lam(vbAlpha, vbAlpha),
            "\\f: alpha -> beta . \\a: alpha . f a" -> lam(vF, lam(vA, app(vF, vA))),
            "\\f: alpha -> beta . \\a: alpha . \\b: beta . f a" -> lam(vF, lam(vA, lam(vB, app(vF, vA)))),
            "\\t: (alpha -> beta) -> alpha. \\b: beta . t (\\a: alpha . b)" -> lam(vT, lam(vB, app(vT, lam(vA, vB)))),
            "\\g: beta -> alpha . \\f: alpha -> beta . \\a: alpha . g (f a)" -> lam(vG, lam(vF, lam(vA, app(vG, app(vF, vA))))),
            "\\f: alpha -> beta . \\g: beta -> alpha . \\a: alpha . f (g (f a))" -> lam(vF, lam(vG, lam(vA, app(vF, app(vG, app(vF, vA)))))),
            "\\h: alpha -> alpha -> alpha . \\a: alpha . h (h a a) (h a a)" -> lam(vH, lam(vA, app(app(vH, app(app(vH, vA), vA)), app(app(vH, vA), vA)))),
        )

        validInputs.foreach { case (input, expected) =>
            val actualOpt = Expression.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Expression.fromString" should "fail on invalid input" in new Wiring {
        val validInputs = List(
            "", // empty string
            "\\a: alpha . ", // no body
            "\\a: alpha . \\a: alpha . a", // double intro
            "\\a: alpha . b", // no variable "b" in context
            "\\: alpha -> beta . \\a: alpha . f a", // no variable
            "\\f alpha -> beta . \\a: alpha . \\b: beta . f a", // no ":"
            "\\t: \\b: beta . t (\\a: alpha . b)", // empty type
            "\\g: beta -> alpha \\f: alpha -> beta . \\a: alpha . g (f a)", // no "."
            "\\f: alpha -> beta . g: beta -> alpha . \\a: alpha . f (g (f a))", // no "\\"
            "\\h: alpha -> alpha -> alpha . \\a: alpha . h (h a ", // invalid body
        )

        validInputs.foreach { input =>
            val actual = Expression.fromString(input)
            assert(actual.isEmpty)
        }
    }

    "getType" should "inference the type by valid expression" in new Wiring {
        val validExpressions: List[(Expression, Type)] = List(
            ("\\a: alpha . a", "alpha -> alpha"),
            ("\\f: alpha -> beta . \\a: alpha . f a", "(alpha -> beta) -> alpha -> beta"),
            ("\\f: alpha -> beta . \\a: alpha . \\b: beta . f a", "(alpha -> beta) -> alpha -> beta -> beta"),
            ("\\t: (alpha -> beta) -> alpha. \\b: beta . t (\\a: alpha . b)", "((alpha -> beta) -> alpha) -> beta -> alpha"),
            ("\\g: beta -> alpha . \\f: alpha -> beta . \\a: alpha . g (f a)", "(beta -> alpha) -> (alpha -> beta) -> alpha -> alpha"),
            ("\\f: alpha -> beta . \\g: beta -> alpha . \\a: alpha . f (g (f a))", "(alpha -> beta) -> (beta -> alpha) -> alpha -> beta"),
            ("\\h: alpha -> alpha -> alpha . \\a: alpha . h (h a a) (h a a)", "(alpha -> alpha -> alpha) -> alpha -> alpha"),
        ).map { case (term, termType) =>
            (Expression.fromString(term).get, Type.fromString(termType).get)
        }

        validExpressions.foreach { case (term, expectedType) =>
            val actualTypeOpt = term.getType
            assert(actualTypeOpt contains expectedType)
        }
    }

    "getType" should "fail on wrong expressions" in new Wiring {
        val invalidExpressions: List[Expression] = List(
            "\\a: alpha . a a",
            "\\f: beta -> alpha . \\a: alpha . f a",
            "\\f: alpha -> beta . \\a: alpha . \\b: beta . f b",
            "\\t: (gamma -> beta) -> alpha. \\b: beta . t (\\a: alpha . b)",
            "\\g: gamma -> alpha . \\f: alpha -> beta . \\a: alpha . g (f a)",
            "\\f: alpha -> beta . \\g: beta -> gamma . \\a: alpha . f (g (f a))",
            "\\h: alpha -> gamma -> alpha . \\a: alpha . h (h a a) (h a a)",
        ).map(Expression.fromString(_).get)

        invalidExpressions.foreach { term =>
            val actualTypeOpt = term.getType
            assert(actualTypeOpt.isEmpty)
        }
    }

    trait Wiring {
        val tcA = TypeConstant("alpha")
        val tcB = TypeConstant("beta")
        val vA = Variable("a", tcA)
        val vbA = Variable("A", tcA)
        val vbAlpha = Variable("Alpha", tcA)
        val vB = Variable("b", tcB)
        val vF = Variable("f", TypeArrow(tcA, tcB))
        val vG = Variable("g", TypeArrow(tcB, tcA))
        val vT = Variable("t", TypeArrow(TypeArrow(tcA, tcB), tcA))
        val vH = Variable("h", TypeArrow(tcA, TypeArrow(tcA, tcA)))

        // functions with short names for convenience

        def lam(variable: Variable, expression: Expression): Abstraction =
            Abstraction(variable, expression)

        def app(left: Expression, right: Expression): Application =
            Application(left, right)
    }
}
