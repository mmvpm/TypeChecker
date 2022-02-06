package types

abstract class Type {

    def toStringWithBrackets = s"($toString)"

    def toStringVerbose: String

    def applyTo(other: Type): Option[Type]

    def substitute(previous: TypeVariable, next: Type): Type
}

object Type {

    def fromString(input: String): Option[Type] = {
        // removing double spaces
        val updatedInput = input.split(' ').filter(_.nonEmpty).mkString(" ")
        Type.fromString(updatedInput, Set.empty)
    }

    def fromString(input: String, typeVariables: Set[String]): Option[Type] = {
        val updatedInput = util.trimBrackets(input)

        // `Lazy` to not call other methods if `nonEmpty` has already been found
        LazyList(
            TypeAbstraction.fromString _,
            TypeArrow.fromString _,
            TypeVariable.fromString _,
            TypeConstant.fromString _
        ).map { constructor =>
            constructor(updatedInput, typeVariables)
        }.find(_.nonEmpty).flatten
    }
}
