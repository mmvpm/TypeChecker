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

    // System F

    "Term.fromString" should "parse valid input to System F Term" in new Wiring {
        val validInputs = List(
            "forall alpha -> \\a: alpha -> a"
                -> ulam(tvA, lam(uvA, uvA)),
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\a: alpha -> f a"
                -> ulam(tvA, ulam(tvB, lam(uvF, lam(uvA, app(uvF, uvA))))),
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\a: alpha -> \\b: beta -> f a"
                -> ulam(tvA, ulam(tvB, lam(uvF, lam(uvA, lam(uvB, app(uvF, uvA)))))),
            "forall alpha -> forall beta -> \\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)"
                -> ulam(tvA, ulam(tvB, lam(uvT, lam(uvB, app(uvT, lam(uvA, uvB)))))),
            "forall alpha -> forall beta -> \\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)"
                -> ulam(tvA, ulam(tvB, lam(uvG, lam(uvF, lam(uvA, app(uvG, app(uvF, uvA))))))),
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))"
                -> ulam(tvA, ulam(tvB, lam(uvF, lam(uvG, lam(uvA, app(uvF, app(uvG, app(uvF, uvA)))))))),
            "forall alpha -> forall beta -> \\h: alpha => alpha => alpha -> \\a: alpha -> h (h a a) (h a a)"
                -> ulam(tvA, ulam(tvB, lam(uvH, lam(uvA, app(app(uvH, app(app(uvH, uvA), uvA)), app(app(uvH, uvA), uvA)))))),
            "forall beta -> \\q: forall alpha . alpha => alpha -> \\b: beta -> q ~ beta b"
                -> ulam(tvB, lam(uvQ, lam(uvB, app(uapp(uvQ, tvB), uvB)))),
            "\\q: forall alpha . alpha => alpha -> \\b: beta -> q ~ beta b"
                -> lam(uvQ, lam(vB, app(uapp(uvQ, tcB), vB))),
            "forall beta -> \\w: forall alpha . beta => alpha -> \\b: beta -> w ~ beta b"
                -> ulam(tvB, lam(uvW, lam(uvB, app(uapp(uvW, tvB), uvB)))),
            "\\p: forall alpha . (mu => alpha) => forall beta . beta => mu -> p ~ mu (\\m: mu -> m) ~ mu"
                -> lam(uvP, uapp(app(uapp(uvP, tcM), lam(vM, vM)), tcM)),
            "\\s: forall alpha . forall beta . beta -> s ~ mu ~ mu"
                -> lam(uvS, uapp(uapp(uvS, tcM), tcM)),
            "\\u: mu => forall alpha . alpha -> \\s: forall alpha . forall beta . beta -> u (s ~ mu) ~ mu"
                -> lam(uvU, lam(uvS, uapp(app(uvU, uapp(uvS, tcM)), tcM)))
        )

        validInputs.foreach { case (input, expected) =>
            val actualOpt = Term.fromString(input)
            assert(actualOpt contains expected)
        }
    }

    "Term.fromString" should "fail on invalid System F input" in new Wiring {
        val invalidInputs = List(
            "forall alpha -> ", // no body
            "forall alpha -> alpha", // type instead of term
            "forall alpha -> \\a: alpha -> alpha", // type instead of term
            "forall  -> \\a: alpha -> a", // no type variable
            "forall alpha -> forall alpha -> \\f: alpha => beta -> \\a: alpha -> f a", // double intro
            "forall alpha -> forall beta -> \\f -> \\a: alpha -> \\b: beta -> f a", // no type
            "forall alpha -> forall beta -> \\f:  -> \\a: alpha -> \\b: beta -> f a", // empty type
            "forall alpha -> forall  -> \\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)", // no variable
            "\\s: forall alpha . forall beta . beta -> s mu mu", // no "~"
            "forall alpha -> forall beta  \\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)", // no "->"
            "forall alpha -> forall beta -> \\g beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)", // no ":"
            "forall alpha -> forall beta -> \\g: beta => alpha \\f: alpha => beta -> \\a: alpha -> g (f a)", // no "->"
            "forall alpha -> forall beta -> \\g: beta  alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)", // no "=>"
            "forall alpha -> forall beta -> f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))", // no "\\"
            "forall alpha ->  beta -> \\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))", // no "forall"
            "forall alpha -> foral beta -> \\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))", // "foral" instead of "forall"
            "forall alpha -> forall beta -> \\h: alpha => alpha => alpha -> \\a: alpha -> h (h a a) (h a", // invalid body
            "forall beta . \\q: forall alpha -> alpha => alpha . \\b: beta -> q ~ beta b", // "." instead of "->"
            "forall beta -> \\q: forall alpha -> alpha => alpha -> \\b: beta -> q ~ beta b", // "->" instead of "."
            "forall beta -> \\w:  alpha . beta => alpha -> \\b: beta -> w ~ beta b", // no "forall" in type
            "\\p: forall alpha . (mu => alpha) => forall beta . beta => mu -> mu ~ p (\\m: mu -> m) ~ mu" // type and term switched
        )

        invalidInputs.foreach { input =>
            val actual = Term.fromString(input)
            assert(actual.isEmpty)
        }
    }

    "getType" should "inference the type by valid System F term" in new Wiring {
        val validTerms: List[(Term, Type)] = List(
            ("forall alpha -> \\a: alpha -> a",
                "forall alpha . alpha => alpha "),
            ("forall alpha -> forall beta -> \\f: alpha => beta -> \\a: alpha -> f a",
                "forall alpha . forall beta . (alpha => beta) => alpha => beta"),
            ("forall alpha -> forall beta -> \\f: alpha => beta -> \\a: alpha -> \\b: beta -> f a",
                "forall alpha . forall beta . (alpha => beta) => alpha => beta => beta"),
            ("forall alpha -> forall beta -> \\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> b)",
                "forall alpha . forall beta . ((alpha => beta) => alpha) => beta => alpha"),
            ("forall alpha -> forall beta -> \\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> g (f a)",
                "forall alpha . forall beta . (beta => alpha) => (alpha => beta) => alpha => alpha"),
            ("forall alpha -> forall beta -> \\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f a))",
                "forall alpha . forall beta . (alpha => beta) => (beta => alpha) => alpha => beta"),
            ("forall alpha -> forall beta -> \\h: alpha => alpha => alpha -> \\a: alpha -> h (h a a) (h a a)",
                "forall alpha . forall beta . (alpha => alpha => alpha) => alpha => alpha"),
            ("forall beta -> \\q: forall alpha . alpha => alpha -> \\b: beta -> q ~ beta b",
                "forall beta . (forall alpha . alpha => alpha) => beta => beta"),
            ("\\q: forall alpha . alpha => alpha -> \\b: beta -> q ~ beta b",
                "(forall alpha . alpha => alpha) => beta => beta"),
            ("forall beta -> \\w: forall alpha . beta => alpha -> \\b: beta -> w ~ beta b",
                "forall beta . (forall alpha . beta => alpha) => beta => beta"),
            ("\\p: forall alpha . (mu => alpha) => forall beta . beta => mu -> p ~ mu (\\m: mu -> m) ~ mu",
                "(forall alpha . (mu => alpha) => forall beta . beta => mu) => (mu => mu)"),
            ("\\s: forall alpha . forall beta . beta -> s ~ mu ~ mu",
                "(forall alpha . forall beta . beta) => mu"),
            ("\\u: mu => forall alpha . alpha -> \\s: forall beta . beta -> u (s ~ mu) ~ mu",
                "(mu => forall alpha . alpha) => (forall beta . beta) => mu")
        ).map { case (term, termType) =>
            (Term.fromString(term).get, Type.fromString(termType).get)
        }

        validTerms.foreach { case (term, expectedType) =>
            val actualTypeOpt = term.getType
            assert(actualTypeOpt contains expectedType)
        }
    }

    "getType" should "fail on wrong System F terms" in new Wiring {
        val invalidTerms: List[Term] = List(
            "forall alpha -> \\a: alpha -> a a",
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\b: beta -> f b",
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\a: alpha -> \\b: beta -> f b",
            "forall alpha -> forall beta -> \\t: (alpha => beta) => alpha -> \\b: beta -> t (\\a: alpha -> a)",
            "forall alpha -> forall beta -> \\g: beta => alpha -> \\f: alpha => beta -> \\a: alpha -> f (g a)",
            "forall alpha -> forall beta -> \\f: alpha => beta -> \\g: beta => alpha -> \\a: alpha -> f (g (f g))",
            "forall beta -> \\q: forall alpha . alpha => alpha -> \\m: mu -> q ~ beta m",
            "\\q: forall alpha . alpha => alpha -> \\b: beta -> q ~ mu b",
            "forall beta -> \\w: forall alpha . gamma => gamma -> \\b: beta -> w ~ beta b",
            "\\p: forall alpha . (mu => alpha) => forall beta . beta => mu -> p ~ mu (\\m: mu -> p) ~ mu"
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

        val tvA = TypeVariable("alpha")
        val tvB = TypeVariable("beta")
        val uvA = Variable("a", tvA)
        val uvB = Variable("b", tvB)
        val uvF = Variable("f", TypeArrow(tvA, tvB))
        val uvG = Variable("g", TypeArrow(tvB, tvA))
        val uvT = Variable("t", TypeArrow(TypeArrow(tvA, tvB), tvA))
        val uvH = Variable("h", TypeArrow(tvA, TypeArrow(tvA, tvA)))
        val uvQ = Variable("q", TypeAbstraction(tvA, TypeArrow(tvA, tvA)))
        val uvW = Variable("w", TypeAbstraction(tvA, TypeArrow(tvB, tvA)))
        val uvP = Variable("p", TypeAbstraction(tvA, TypeArrow(TypeArrow(tcM, tvA), TypeAbstraction(tvB, TypeArrow(tvB, tcM)))))
        val uvS = Variable("s", TypeAbstraction(tvA, TypeAbstraction(tvB, tvB)))
        val uvU = Variable("u", TypeArrow(tcM, TypeAbstraction(tvA, tvA)))

        // functions with short names for convenience

        def app: (Term, Term) => Application = Application(_, _)

        def lam: (Variable, Term) => Abstraction = Abstraction(_, _)

        def uapp: (Term, Type) => UniversalApplication = UniversalApplication(_, _)

        def ulam: (TypeVariable, Term) => UniversalAbstraction = UniversalAbstraction(_, _)
    }
}
