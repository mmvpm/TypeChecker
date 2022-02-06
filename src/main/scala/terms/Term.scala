package terms

import types._

abstract class Term {

    def getType: Option[Type]

    def toStringWithBrackets = s"($toString)"

    def toStringVerbose: String
}

object Term {

    def fromString(input: String): Option[Term] = {
        // removing double spaces
        val updatedInput = input.split(' ').filter(_.nonEmpty).mkString(" ")
        Term.fromString(updatedInput, Map.empty, Set.empty)
    }

    def fromString(
        input: String,
        context: Map[String, Type], // known variable name -> its type
        typeVariables: Set[String] // known type variable names
    ): Option[Term] = {
        val updatedInput = util.trimBrackets(input)

        // `Lazy` to not call other methods if `nonEmpty` has already been found
        LazyList(
            UniversalAbstraction.fromString _,
            Abstraction.fromString _,
            Variable.fromString _,
            Application.fromString _,
        ).map { constructor =>
            constructor(updatedInput, context, typeVariables)
        }.find(_.nonEmpty).flatten
    }
}
