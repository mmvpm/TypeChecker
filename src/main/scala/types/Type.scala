package types

abstract class Type {

    def toStringWithBrackets = s"($toString)"

    def toStringVerbose: String

    def applyTo(other: Type): Option[Type]
}

object Type {

    def fromString(input: String): Option[Type] = {
        val updatedInput = util.trimBrackets(input)

        // `Lazy` to not call other methods if `nonEmpty` has already been found
        LazyList(
            TypeArrow.fromString _,
            TypeConstant.fromString _
        ).map { constructor =>
            constructor(updatedInput)
        }.find(_.nonEmpty).flatten
    }
}
