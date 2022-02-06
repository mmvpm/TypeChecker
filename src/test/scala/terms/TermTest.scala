package terms

import org.scalatest.flatspec.AnyFlatSpec

import types._

class TermTest extends AnyFlatSpec {

    "Term.fromString" should "parse valid input to Term" in new Wiring {
        val validInputs = List(
            "\\a: alpha -> a" -> lam(vA, vA),
            "\\A: alpha -> A" -> lam(vbA, vbA),
            "\\Alpha: alpha -> Alpha" -> lam(vbAlpha, vbAlpha),
            "\\f: alpha => beta -> \\a: alpha -> f a" -> lam(vF, lam(vA, app(vF, vA))),
            "\\f: alpha => beta -> \\a: alpha -> \\b: beta -> f a" -> lam(vF, lam(vA, lam(vB, app(vF, vA)))),
            "\\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)" -> lam(vT, lam(vB, app(vT, lam(vA, vB)))),
            "\\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)" -> lam(vG, lam(vF, lam(vA, app(vG, app(vF, vA))))),
            "\\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))" -> lam(vF, lam(vG, lam(vA, app(vF, app(vG, app(vF, vA)))))),
            "\\h: alpha => alpha => alpha -> \\a: alpha -> h (h a a) (h a a)" -> lam(vH, lam(vA, app(app(vH, app(app(vH, vA), vA)), app(app(vH, vA), vA)))),
        )

        validInputs.foreach { case (input, expected) =>
            val actualOpt = Term.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Term.fromString" should "fail on invalid input" in new Wiring {
        val invalidInputs = List(
            "", // empty string
            "\\a: alpha -> ", // no body
            "\\a: alpha -> \\a: alpha -> a", // double intro
            "\\a: alpha -> b", // no variable "b" in context
            "\\: alpha => beta -> \\a: alpha -> f a", // no variable
            "\\f alpha => beta -> \\a: alpha -> \\b: beta -> f a", // no ":"
            "\\t: \\b: beta -> t (\\a: alpha -> b)", // empty type
            "\\g: beta => alpha \\f: alpha => beta -> \\a: alpha -> g (f a)", // no "->"
            "\\f: alpha => beta -> g: beta => alpha -> \\a: alpha -> f (g (f a))", // no "\\"
            "\\h: alpha => alpha => alpha -> \\a: alpha -> h (h a ", // invalid body
        )

        invalidInputs.foreach { input =>
            val actual = Term.fromString(input)
            assert(actual.isEmpty)
        }
    }

    "getType" should "inference the type by valid term" in new Wiring {
        val validTerms: List[(Term, Type)] = List(
            ("\\a: alpha -> a", "alpha => alpha"),
            ("\\f: alpha => beta -> \\a: alpha -> f a", "(alpha => beta) => alpha => beta"),
            ("\\f: alpha => beta -> \\a: alpha -> \\b: beta -> f a", "(alpha => beta) => alpha => beta => beta"),
            ("\\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)", "((alpha => beta) => alpha) => beta => alpha"),
            ("\\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)", "(beta => alpha) => (alpha => beta) => alpha => alpha"),
            ("\\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))", "(alpha => beta) => (beta => alpha) => alpha => beta"),
            ("\\h: alpha => alpha => alpha -> \\a: alpha -> h (h a a) (h a a)", "(alpha => alpha => alpha) => alpha => alpha"),
        ).map { case (term, termType) =>
            (Term.fromString(term).get, Type.fromString(termType).get)
        }

        validTerms.foreach { case (term, expectedType) =>
            val actualTypeOpt = term.getType
            assert(actualTypeOpt contains expectedType)
        }
    }

    "getType" should "fail on wrong terms" in new Wiring {
        val invalidTerms: List[Term] = List(
            "\\a: alpha -> a a",
            "\\f: beta => alpha -> \\a: alpha -> f a",
            "\\f: alpha => beta -> \\a: alpha -> \\b: beta -> f b",
            "\\t: (gamma => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)",
            "\\g: gamma => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)",
            "\\f: alpha => beta -> \\g: beta => gamma -> \\a: alpha -> f (g (f a))",
            "\\h: alpha => gamma => alpha -> \\a: alpha -> h (h a a) (h a a)",
        ).map(Term.fromString(_).get)

        invalidTerms.foreach { term =>
            val actualTypeOpt = term.getType
            assert(actualTypeOpt.isEmpty)
        }
    }

    trait Wiring {
        val tcA = TypeConstant("alpha")
        val tcB = TypeConstant("beta")
        val tcM = TypeConstant("mu")
        val vA = Variable("a", tcA)
        val vbA = Variable("A", tcA)
        val vbAlpha = Variable("Alpha", tcA)
        val vB = Variable("b", tcB)
        val vM = Variable("m", tcM)
        val vF = Variable("f", TypeArrow(tcA, tcB))
        val vG = Variable("g", TypeArrow(tcB, tcA))
        val vT = Variable("t", TypeArrow(TypeArrow(tcA, tcB), tcA))
        val vH = Variable("h", TypeArrow(tcA, TypeArrow(tcA, tcA)))

        // functions with short names for convenience

        def app: (Term, Term) => Application = Application(_, _)

        def lam: (Variable, Term) => Abstraction = Abstraction(_, _)
    }
}
