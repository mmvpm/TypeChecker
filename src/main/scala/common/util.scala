import scala.collection.mutable

package object util {

    def validateName(name: String): Boolean =
        name.nonEmpty && !name.exists(".:-=>( )" contains _)

    def trimBrackets(input: String): String = {
        val updatedInput = input.trim
        if (updatedInput.isEmpty)
            return ""
        if (updatedInput.head == '(' && updatedInput.last == ')') {
            val trimmed = updatedInput.drop(1).dropRight(1)
            if (isBalanced(trimmed))
                return trimBrackets(trimmed)
        }
        updatedInput
    }

    def isBalanced(sequence: String): Boolean = {
        if (sequence == "")
            return true
        val balances = computeBalances(sequence)
        balances.last == 0 && balances.min == 0
    }

    def computeBalances(sequence: String): List[Int] = {
        val balances = mutable.Buffer.empty[Int]
        for (symbol <- sequence) {
            balances += balances.lastOption.getOrElse(0) + bracketCost(symbol)
        }
        balances.toList
    }

    private def bracketCost(symbol: Char): Int = symbol match {
        case '(' => 1
        case ')' => -1
        case _ => 0
    }
}
