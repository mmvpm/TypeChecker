package common

import org.scalatest.flatspec.AnyFlatSpec

class UtilTest extends AnyFlatSpec {

    "computeBalance" should "compute balances correctly for balances sequences" in new Wiring {
        balancedSequences.foreach { case (sequence, expected) =>
            val actual = util.computeBalances(sequence)
            assert(actual == expected)
        }
    }

    "computeBalance" should "compute balances correctly for dirty balances sequences" in new Wiring {
        balancedSequencesDirty.foreach { case (sequence, expected) =>
            val actual = util.computeBalances(sequence)
            assert(actual == expected)
        }
    }

    "computeBalance" should "compute balances correctly for unbalances sequences" in new Wiring {
        unbalancedSequences.foreach { case (sequence, expected) =>
            val actual = util.computeBalances(sequence)
            assert(actual == expected)
        }
    }

    "computeBalance" should "compute balances correctly for dirty unbalances sequences" in new Wiring {
        unbalancedSequencesDirty.foreach { case (sequence, expected) =>
            val actual = util.computeBalances(sequence)
            assert(actual == expected)
        }
    }

    "isBalanced" should "return true for balances sequences" in new Wiring {
        balancedSequences.foreach { case (sequence, _) =>
            val actual = util.isBalanced(sequence)
            assert(actual)
        }
    }

    "isBalanced" should "return true for dirty balances sequences" in new Wiring {
        balancedSequencesDirty.foreach { case (sequence, _) =>
            val actual = util.isBalanced(sequence)
            assert(actual)
        }
    }

    "isBalanced" should "return false for unbalances sequences" in new Wiring {
        unbalancedSequences.foreach { case (sequence, _) =>
            val actual = util.isBalanced(sequence)
            assert(!actual)
        }
    }

    "isBalanced" should "return false for dirty unbalances sequences" in new Wiring {
        unbalancedSequencesDirty.foreach { case (sequence, _) =>
            val actual = util.isBalanced(sequence)
            assert(!actual)
        }
    }

    "trimBrackets" should "trim brackets correctly" in {
        val testCases = List(
            "a" -> "a",
            "(a)" -> "a",
            ")a(" -> ")a(",
            "(( a ))" -> "a",
            "( ( a ) )" -> "a",
            "  ( ( a ) )  " -> "a",
            " (a) (b) " -> "(a) (b)",
            "((a)(b))" -> "(a)(b)",
            ")(a)(b))" -> ")(a)(b))",
            "((a)(b)(" -> "((a)(b)(",
            " ( ( ( alpha ) ) ) " -> "alpha",
        )
        testCases.foreach { case (input, expected) =>
            val actual = util.trimBrackets(input)
            assert(actual == expected)
        }
    }

    trait Wiring {
        val balancedSequences = List(
            "" -> List.empty,
            "()" -> List(1, 0),
            "(())" -> List(1, 2, 1, 0),
            "()()" -> List(1, 0, 1, 0),
            "(())()" -> List(1, 2, 1, 0, 1, 0),
            "()(())" -> List(1, 0, 1, 2, 1, 0),
            "()((()))()(())()" -> List(1, 0, 1, 2, 3, 2, 1, 0, 1, 0, 1, 2, 1, 0, 1, 0)
        )
        val balancedSequencesDirty = List(
            " a " -> List(0, 0, 0),
            " a (a a) " -> List(0, 0, 0, 1, 1, 1, 1, 0, 0),
            " a (  (a)  )" -> List(0, 0, 0, 1, 1, 1, 2, 2, 1, 1, 1, 0),
            "a ()() b" -> List(0, 0, 1, 0, 1, 0, 0, 0),
            "(a(b)c)(d)" -> List(1, 1, 2, 2, 1, 1, 0, 1, 1, 0),
            " ( ) ( ( ) ) " -> List(0, 1, 1, 0, 0, 1, 1, 2, 2, 1, 1, 0, 0),
            " (a) ( (% (-) => )-> ).(forall)(:($) ):(\\)" -> List(0, 1, 1, 0, 0, 1, 1, 2, 2, 2, 3, 3, 2, 2, 2, 2, 2, 1, 1, 1, 1, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 1, 1, 2, 2, 1, 1, 0, 0, 1, 1, 0)
        )
        val unbalancedSequences = List(
            "(" -> List(1),
            ")" -> List(-1),
            ")(" -> List(-1, 0),
            "())" -> List(1, 0, -1),
            "(()" -> List(1, 2, 1),
            "(()()" -> List(1, 2, 1, 2, 1),
            ")(())" -> List(-1, 0, 1, 0, -1),
            "()((())))(())()" -> List(1, 0, 1, 2, 3, 2, 1, 0, -1, 0, 1, 0, -1, 0, -1)
        )
        val unbalancedSequencesDirty = List(
            "( " -> List(1, 1),
            " )" -> List(0, -1),
            " ) a ( " -> List(0, -1, -1, -1, -1, 0, 0),
            " ( ) ) " -> List(0, 1, 1, 0, 0, -1, -1),
            "(a(b )" -> List(1, 1, 2, 2, 2, 1),
            "a(b(c)d(e)f" -> List(0, 1, 1, 2, 2, 1, 1, 2, 2, 1, 1),
            ")a ((.)) alpha" -> List(-1, -1, -1, 0, 1, 1, 0, -1, -1, -1, -1, -1, -1, -1),
            "a(.):(%(@(-)->)=>) ):(.( )forall)( alpha ) " -> List(0, 1, 1, 0, 0, 1, 1, 2, 2, 3, 3, 2, 2, 2, 1, 1, 1, 0, 0, -1, -1, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, -1, -1)
        )
    }
}
